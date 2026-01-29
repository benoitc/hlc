# Changelog

## 3.0.2 - 2026-01-29

- fix format string bug in error logging (use ~p instead of %p)
- fix incorrect type specs for now/1 and update/2
- fix extra parentheses in start_link/3
- fix typos in doc comments
- add missing gen_server callbacks (handle_info, terminate, code_change)
- add stop_manual_clock/1 to prevent process leak
- add logical counter overflow protection

## 3.0.1

- fix hex.pm metadata

## 3.0.0

- modernize for OTP 21+

## 0.1.0 - 2014/11/14

- initial release
