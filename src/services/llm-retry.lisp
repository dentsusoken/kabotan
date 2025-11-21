(in-package :kabotan)

;;; LLM Retry Logic
;;; Retry wrapper functions and message validation for LLM API calls

(defun validate-message-role (role)
  "Validates that a message role is one of the allowed values.
   
   Parameters:
   - role: The role string to validate
   
   Returns:
   - t if role is valid (system, user, or assistant)
   - nil otherwise"
  (member role '("system" "user" "assistant") :test #'string=))

(defun messages-to-json-array (messages)
  "Converts a list of message plists to a JSON-compatible alist structure.
   
   Parameters:
   - messages: List of plists with :role and :content keys
               Example: ((:role \"user\" :content \"Hello\") (:role \"assistant\" :content \"Hi!\"))
   
   Returns:
   - List of alists suitable for JSON encoding
   - nil if any message has invalid structure"
  (loop for msg in messages
        for role = (getf msg :role)
        for content = (getf msg :content)
        
        ;; Validate message structure
        unless (and role content (stringp role) (stringp content))
          do (progn
               (log-error "Invalid message structure" 
                         (format nil "Message missing role or content: ~A" msg))
               (return-from messages-to-json-array nil))
        
        ;; Validate role
        unless (validate-message-role role)
          do (progn
               (log-error "Invalid message role" 
                         (format nil "Role '~A' is not valid. Must be system, user, or assistant" role))
               (return-from messages-to-json-array nil))
        
        ;; Build JSON-compatible alist
        collect `(("role" . ,role)
                 ("content" . ,content))))

(defun call-llm (prompt &key (max-tokens 10000) (temperature 0.7) (timeout 360))
  "Calls the LLM service with the given prompt and returns the response text.
   Handles timeouts and errors, returning nil on failure.
   
   Parameters:
   - prompt: The text prompt to send to the LLM
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   - timeout: Maximum time in seconds to wait for response (default: 360, ~6 minutes)
   
   Returns:
   - Response text string on success
   - nil on failure (timeout or error)"
  ;; Check if a test mock function is bound
  (if *llm-api-function*
      (funcall *llm-api-function* prompt max-tokens temperature timeout)
      ;; Otherwise use our custom API call
      (call-openai-api prompt 
                       :max-tokens max-tokens 
                       :temperature temperature 
                       :timeout timeout)))

(defun call-llm-with-retry (prompt &key (max-retries 5) (timeout 360) (max-tokens 10000) (temperature 0.7))
  "Calls LLM with retry logic and exponential backoff for resilience.
   
   Parameters:
   - prompt: The text prompt to send to the LLM
   - max-retries: Maximum number of retry attempts (default: 5)
   - timeout: Maximum time in seconds to wait for each attempt (default: 360, ~6 minutes)
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   
   Returns:
   - Response text string on success
   - nil on failure after all retries exhausted"
  (let ((attempt 0)
        (backoff-seconds 5))
    (loop
      (incf attempt)
      
      ;; Try to call LLM
      (let ((response (call-llm prompt 
                                :max-tokens max-tokens 
                                :temperature temperature 
                                :timeout timeout)))
        (when response
          (return-from call-llm-with-retry response)))
      
      ;; If we've exhausted retries, give up
      (when (>= attempt max-retries)
        (log-error "LLM retry exhausted" 
                  (format nil "Failed after ~A attempts" max-retries))
        (return-from call-llm-with-retry nil))
      
      ;; Log retry attempt with structured logging
      (log-llm-retry attempt max-retries backoff-seconds "api-failure")
      
      ;; Also log to existing error log for backward compatibility
      (log-error "LLM retry" 
                (format nil "Attempt ~A failed, retrying in ~A seconds" 
                        attempt backoff-seconds))
      
      ;; Wait with exponential backoff before retrying (max 60 seconds for long-running LLM requests)
      (sleep backoff-seconds)
      (setf backoff-seconds (min 60 (* backoff-seconds 2))))))

(defun call-llm-with-messages (messages &key (max-tokens 10000) (temperature 0.7) (timeout 360) system-prompt)
  "Calls the LLM service with a message array and returns the response text.
   Handles timeouts and errors, returning nil on failure.
   
   Parameters:
   - messages: List of message plists with :role and :content keys
               Example: ((:role \"user\" :content \"Hello\") (:role \"assistant\" :content \"Hi!\"))
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   - timeout: Maximum time in seconds to wait for response (default: 360, ~6 minutes)
   - system-prompt: Optional system message to prepend (string)
   
   Returns:
   - Response text string on success
   - nil on failure (timeout or error)"
  ;; Check if a test mock function is bound
  (if *llm-api-messages-function*
      ;; Use mock function for message-based API
      (funcall *llm-api-messages-function* 
               messages
               max-tokens 
               temperature 
               timeout
               system-prompt)
      ;; Otherwise use real API
      (progn
        ;; Prepend system prompt if provided
        (let ((final-messages (if (and system-prompt 
                                      (stringp system-prompt) 
                                      (> (length system-prompt) 0))
                                 (cons (list :role "system" :content system-prompt)
                                       messages)
                                 messages)))
          
          ;; Call API with messages
          (call-openai-api-with-messages final-messages
                                         :max-tokens max-tokens
                                         :temperature temperature
                                         :timeout timeout)))))

(defun call-llm-with-messages-retry (messages &key (max-retries 5) (timeout 360) (max-tokens 10000) (temperature 0.7) system-prompt)
  "Calls LLM with message array using retry logic and exponential backoff for resilience.
   
   Parameters:
   - messages: List of message plists with :role and :content keys
   - max-retries: Maximum number of retry attempts (default: 5)
   - timeout: Maximum time in seconds to wait for each attempt (default: 360, ~6 minutes)
   - max-tokens: Maximum number of tokens in the response (default: 10000)
   - temperature: Sampling temperature for response generation (default: 0.7)
   - system-prompt: Optional system message to prepend (string)
   
   Returns:
   - Response text string on success
   - nil on failure after all retries exhausted"
  (let ((attempt 0)
        (backoff-seconds 5))
    (loop
      (incf attempt)
      
      ;; Try to call LLM with messages
      (let ((response (call-llm-with-messages messages
                                              :max-tokens max-tokens 
                                              :temperature temperature 
                                              :timeout timeout
                                              :system-prompt system-prompt)))
        (when response
          (return-from call-llm-with-messages-retry response)))
      
      ;; If we've exhausted retries, give up
      (when (>= attempt max-retries)
        (log-error "LLM retry exhausted" 
                  (format nil "Failed after ~A attempts" max-retries))
        (return-from call-llm-with-messages-retry nil))
      
      ;; Log retry attempt with structured logging
      (log-llm-retry attempt max-retries backoff-seconds "api-failure")
      
      ;; Also log to existing error log for backward compatibility
      (log-error "LLM retry" 
                (format nil "Attempt ~A failed, retrying in ~A seconds" 
                        attempt backoff-seconds))
      
      ;; Wait with exponential backoff before retrying (max 60 seconds for long-running LLM requests)
      (sleep backoff-seconds)
      (setf backoff-seconds (min 60 (* backoff-seconds 2))))))
