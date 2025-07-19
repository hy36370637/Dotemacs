;;; my-mu4e.el --- 개인 mu4e 설정

(use-package mu4e
  :load-path "/opt/homebrew/Cellar/mu/1.12.11/share/emacs/site-lisp/mu/mu4e"
  ;; :defer t
  :commands (mu4e)
  :init
  ;; 메일 주소 지정 (기본값)
  (setq user-mail-address "hy36370637@gmail.com")

  ;; mu4e-compose-set-from-header 사용 위해 명시적으로 로드
  (require 'mu4e-compose)

  ;; 메일 저장 디렉토리 (기본값)
  (setq mu4e-maildir "/Users/hykim/Mail")

  ;; 메일 동기화 및 인덱싱
  (setq mu4e-get-mail-command "mbsync -a"
        mu4e-update-interval 300
        mu4e-index-cleanup t
        mu4e-index-lazy-check t
        mu4e-index-updated-automatically t)

  ;; 메일 전송 (msmtp 또는 smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        send-mail-function 'smtpmail-send-it
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587
        smtpmail-stream-type 'starttls)

  :config
  ;; mu4e context 설정
  (setq mu4e-contexts
        (list
          ;; --- Gmail 컨텍스트 ---
          (make-mu4e-context
            :name "Gmail"
            :enter-func (lambda () (mu4e-message "Switched to Gmail context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
            :vars '((user-full-name . "Ho Young")
                    (mu4e-user-mail-address-list . ("hy36370637@gmail.com"))
                    (mu4e-compose-signature .
                      "
--
Ho Young (Gmail)")
                    (mu4e-sent-folder . "/gmail/[Gmail].보낸편지함")
                    (mu4e-drafts-folder . "/gmail/[Gmail].임시보관함")
                    (mu4e-trash-folder . "/gmail/[Gmail].휴지통")
                    (mu4e-refile-folder . "/gmail/[Gmail].전체보관함")
                    (mu4e-compose-custom-header-alist . (("From" . "Ho Young <hy36370637@gmail.com>")))
                    (mu4e-compose-policy-args . ("--account=gmail"))
                    ;; 필요 시 아래 주석 해제
                    ;; (mu4e-compose-pre-hook . (mu4e-compose-set-from-header))
                    ))

          ;; --- Apple Mail 컨텍스트 추가 ---
          ;; 이 컨텍스트 블록 전체를 기존 'Gmail' 컨텍스트 아래에 추가합니다.
          (make-mu4e-context
            :name "Apple Mail"
            :enter-func (lambda () (mu4e-message "Switched to Apple Mail context"))
            ;; Apple Mail 메일 디렉토리 경로에 맞게 수정해주세요.
            ;; `mbsyncrc`의 `MaildirStore` 경로에 `/applemail`로 지정했다면 이대로 둡니다.
            :match-func (lambda (msg)
                          (when msg
                            (string-prefix-p "/applemail" (mu4e-message-field msg :maildir))))
            :vars '((user-full-name . "Ho Young") ;; Apple Mail에서 사용할 이름
                    (user-mail-address . "under9@icloud.com") ;
                    (mu4e-user-mail-address-list . ("under9@icloud.com")) 
                    (mu4e-compose-signature .
                      "
--
Ho Young (Apple Mail)")
                    ;; Apple Mail의 실제 폴더 경로 (mbsync 설정과 일치해야 함)
                    ;; `mbsync -l applemail` 명령으로 정확한 IMAP 폴더명 확인 권장
                    (mu4e-sent-folder . "/applemail/Sent Messages")
                    (mu4e-drafts-folder . "/applemail/Drafts")
                    (mu4e-trash-folder . "/applemail/Trash")
                    (mu4e-refile-folder . "/applemail/Archive") ;; iCloud Mail의 기본 보관함 폴더명
                    (mu4e-compose-custom-header-alist . (("From" . "Ho Young  <under9@icloud.com>"))) 
                    ;; msmtp 사용 시, ~/.msmtprc의 'applemail' 계정 사용 지정
                    (mu4e-compose-policy-args . ("--account=applemail"))
                    ))
          )) ;; 이 괄호는 mu4e-contexts 리스트의 닫는 괄호입니다.

  ;; 기타 UI 설정
  (setq mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-headers-skip-duplicates t
        mu4e-compose-org-mode nil
        mu4e-display-buffer-regexp ".*mu4e.*"))

(provide 'my-mu4e)
;;; my-mu4e.el ends here
