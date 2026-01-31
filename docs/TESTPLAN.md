# COBOL Account Management System - Test Plan

## Overview

This test plan validates the business logic and functionality of the COBOL Account Management System. It covers all menu operations, business rules, edge cases, and error handling scenarios.

**Test Environment:** COBOL Application (cobc compiler)  
**Test Scope:** All three COBOL programs (main.cob, operations.cob, data.cob)  
**Test Date:** TBD  
**Tester:** TBD  

---

## Test Cases

| Test Case ID | Test Case Description | Pre-conditions | Test Steps | Expected Result | Actual Result | Status | Comments |
|---|---|---|---|---|---|---|---|
| TC-001 | View initial account balance | Program started, no transactions performed | 1. Run application<br/>2. Select option 1 (View Balance) | Display current balance: 1000.00 | | | Validates initial balance is 1000.00 |
| TC-002 | View balance after credit operation | Program started | 1. Select option 1 to view balance<br/>2. Note initial balance (1000.00)<br/>3. Verify display shows correct value | Current balance is displayed accurately | | | Confirms balance display functionality |
| TC-003 | Credit account with valid amount | Program started, balance = 1000.00 | 1. Select option 2 (Credit Account)<br/>2. Enter amount: 500.00<br/>3. View balance to verify | New balance: 1500.00, Display "Amount credited. New balance: 1500.00" | | | Tests credit operation success |
| TC-004 | Credit account with small amount | Program started, balance = 1000.00 | 1. Select option 2<br/>2. Enter amount: 0.01<br/>3. View balance | New balance: 1000.01 | | | Validates decimal precision in credits |
| TC-005 | Credit account with large amount | Program started, balance = 1000.00 | 1. Select option 2<br/>2. Enter amount: 99999.99<br/>3. View balance | New balance: 100999.99 | | | Tests upper boundary for credit amounts |
| TC-006 | Credit account with zero amount | Program started, balance = 1000.00 | 1. Select option 2<br/>2. Enter amount: 0.00<br/>3. View balance | New balance: 1000.00 (no change) | | | Tests zero credit transaction |
| TC-007 | Multiple consecutive credits | Program started, balance = 1000.00 | 1. Credit 250.00 (balance = 1250.00)<br/>2. Credit 150.00 (balance = 1400.00)<br/>3. Credit 100.00 (balance = 1500.00)<br/>4. View balance | Final balance: 1500.00, All credits processed successfully | | | Validates sequential credit operations |
| TC-008 | Debit account with sufficient funds | Program started, balance = 1000.00 | 1. Select option 3 (Debit Account)<br/>2. Enter amount: 300.00<br/>3. View balance to verify | New balance: 700.00, Display "Amount debited. New balance: 700.00" | | | Tests debit operation success |
| TC-009 | Debit account with insufficient funds | Program started, balance = 1000.00 | 1. Select option 3<br/>2. Enter amount: 1500.00<br/>3. View balance | Display "Insufficient funds for this debit.", Balance remains: 1000.00 | | | **Critical Rule:** Validates overdraft protection |
| TC-010 | Debit account exact balance amount | Program started, balance = 1000.00 | 1. Select option 3<br/>2. Enter amount: 1000.00<br/>3. View balance | New balance: 0.00, Display "Amount debited. New balance: 0.00" | | | Tests boundary condition (exact balance) |
| TC-011 | Debit account with zero balance | Program started after debit of 1000.00, balance = 0.00 | 1. Select option 3<br/>2. Enter amount: 0.01 | Display "Insufficient funds for this debit.", Balance remains: 0.00 | | | Tests debit when balance is zero |
| TC-012 | Debit with small amount | Program started, balance = 1000.00 | 1. Select option 3<br/>2. Enter amount: 0.01<br/>3. View balance | New balance: 999.99 | | | Validates decimal precision in debits |
| TC-013 | Debit with maximum amount | Program started, balance = 1000.00 | 1. Select option 3<br/>2. Enter amount: 999.99<br/>3. View balance | New balance: 0.01, Display "Amount debited. New balance: 0.01" | | | Tests near-maximum debit |
| TC-014 | Multiple consecutive debits (all successful) | Program started, balance = 1000.00 | 1. Debit 200.00 (balance = 800.00)<br/>2. Debit 150.00 (balance = 650.00)<br/>3. Debit 100.00 (balance = 550.00)<br/>4. View balance | Final balance: 550.00, All debits processed successfully | | | Validates sequential debit operations |
| TC-015 | Debit after credit operations | Program started, balance = 1000.00 | 1. Credit 500.00 (balance = 1500.00)<br/>2. Credit 300.00 (balance = 1800.00)<br/>3. Debit 400.00 (balance = 1400.00)<br/>4. View balance | Final balance: 1400.00 | | | Tests mixed credit and debit operations |
| TC-016 | Debit with insufficient funds after credits | Program started, balance = 1000.00 | 1. Credit 100.00 (balance = 1100.00)<br/>2. Debit 1500.00 | Display "Insufficient funds for this debit.", Balance remains: 1100.00 | | | Validates overdraft protection after credits |
| TC-017 | Menu option 1 displays correct balance | Program started | 1. Select menu option 1 | Main menu displayed with "1. View Balance" visible | | | Tests menu display |
| TC-018 | Menu option 2 displays credit prompt | Program started | 1. Select menu option 2<br/>2. View prompt message | Display "Enter credit amount:" | | | Tests credit menu option |
| TC-019 | Menu option 3 displays debit prompt | Program started | 1. Select menu option 3<br/>2. View prompt message | Display "Enter debit amount:" | | | Tests debit menu option |
| TC-020 | Menu option 4 exits application | Program started | 1. Select menu option 4<br/>2. Observe program state | Display "Exiting the program. Goodbye!" and program terminates | | | Tests exit functionality |
| TC-021 | Invalid menu choice - 0 | Program started, main menu displayed | 1. Enter: 0<br/>2. Observe message | Display "Invalid choice, please select 1-4.", Menu displayed again | | | Tests invalid input handling |
| TC-022 | Invalid menu choice - 5 | Program started, main menu displayed | 1. Enter: 5<br/>2. Observe message | Display "Invalid choice, please select 1-4.", Menu displayed again | | | Tests out-of-range input |
| TC-023 | Invalid menu choice - 99 | Program started, main menu displayed | 1. Enter: 99<br/>2. Observe message | Display "Invalid choice, please select 1-4.", Menu displayed again | | | Tests large out-of-range input |
| TC-024 | Menu reappears after operation | Program started | 1. Select option 1<br/>2. Observe if menu appears again | Main menu displayed again after operation completion | | | Tests menu loop functionality |
| TC-025 | Menu continues until exit | Program started | 1. Select option 1<br/>2. Select option 1 again<br/>3. Select option 4 | Menu appears multiple times until option 4 selected | | | Tests continuous loop until exit |
| TC-026 | Balance persistence within session | Program started | 1. Credit 250.00<br/>2. Select option 1<br/>3. Debit 100.00<br/>4. Select option 1 | Balance reflects all previous operations: 1150.00 | | | Tests balance state across operations |
| TC-027 | Application welcome message | Program started | 1. Run application | Display "Account Management System" header | | | Tests initial display |
| TC-028 | Menu separator displayed | Program started | 1. Run application | Display "--------------------------------" separators in menu | | | Tests menu formatting |
| TC-029 | Credit operation completes successfully | Program started | 1. Select option 2<br/>2. Enter 500.00<br/>3. Observe completion | Operation completes and returns to menu | | | Tests operation completion |
| TC-030 | Debit operation completes successfully | Program started | 1. Select option 3<br/>2. Enter 300.00<br/>3. Observe completion | Operation completes and returns to menu | | | Tests debit completion |
| TC-031 | Balance display format correct | Program started | 1. View balance | Balance displayed with exactly 2 decimal places | | | Validates decimal format (XX.XX) |
| TC-032 | Credit amount input acceptance | Program started | 1. Select option 2<br/>2. Enter: 123.45 | System accepts the amount and processes it | | | Tests numeric input parsing |
| TC-033 | Debit amount input acceptance | Program started | 1. Select option 3<br/>2. Enter: 456.78 | System accepts the amount and processes it | | | Tests numeric input parsing for debits |
| TC-034 | Maximum balance not exceeded | Program started, balance = 1000.00 | 1. Attempt credit of 999,000.00<br/>2. Attempt credit of 1.00 | First credit accepted (total 1,000,000), but may exceed PIC 9(6)V99 limit | | | Tests maximum balance constraint |
| TC-035 | Error state recovery | Program started | 1. Select option 3<br/>2. Enter 2000.00 (insufficient funds)<br/>3. Select option 1 | System displays error, recovers, and menu works normally | | | Tests error recovery |
| TC-036 | Overdraft protection - exact boundary | Program started, balance = 100.00 | 1. Select option 3<br/>2. Enter 100.01 | Display "Insufficient funds for this debit.", Balance remains: 100.00 | | | Tests overdraft boundary condition |
| TC-037 | Overdraft protection - negative result prevention | Program started, balance = 500.00 | 1. Select option 3<br/>2. Enter 500.01 | Display "Insufficient funds for this debit.", Balance remains: 500.00, Balance never goes negative | | | **Critical Rule:** Validates negative balance prevention |
| TC-038 | Sequential view operations | Program started | 1. Select option 1<br/>2. Select option 1<br/>3. Select option 1<br/>4. Verify balance same in all views | All three views display same balance | | | Tests consistency of balance reads |
| TC-039 | Balance update persistence after successful debit | Program started, balance = 1000.00 | 1. Debit 200.00 (balance = 800.00)<br/>2. Immediately view balance<br/>3. Select option 3 again | Balance shown as 800.00, next debit starts from 800.00 | | | Validates persistent write operation |
| TC-040 | Initial state each program start | Program restarted (new execution) | 1. Start application<br/>2. Select option 1 to view balance | Balance displayed as 1000.00 | | | Tests initialization on startup |

---

## Test Summary

### Coverage by Operation Type

| Operation | Test Cases | Coverage |
|---|---|---|
| View Balance | TC-001, TC-002, TC-027, TC-038 | 4 |
| Credit Account | TC-003, TC-004, TC-005, TC-006, TC-007, TC-032 | 6 |
| Debit Account - Success | TC-008, TC-010, TC-012, TC-013, TC-014, TC-015, TC-033 | 7 |
| Debit Account - Failure (Overdraft) | TC-009, TC-011, TC-016, TC-021, TC-036, TC-037 | 6 |
| Menu Navigation | TC-017, TC-018, TC-019, TC-020, TC-021, TC-022, TC-023 | 7 |
| Error Handling | TC-021, TC-022, TC-023, TC-035, TC-036, TC-037 | 6 |
| State Management | TC-026, TC-038, TC-039 | 3 |
| System Operations | TC-024, TC-025, TC-028, TC-029, TC-030, TC-040 | 6 |

### Critical Business Rules Validation

| Business Rule | Test Cases | Status |
|---|---|---|
| Initial Balance = $1,000.00 | TC-001, TC-040 | |
| Overdraft Protection (No negative balances) | TC-009, TC-011, TC-016, TC-037 | |
| Credit Operations Always Succeed | TC-003, TC-007, TC-015 | |
| Debit Operations Validate Sufficient Funds | TC-009, TC-010, TC-011, TC-016, TC-036 | |
| Balance Precision = 2 Decimals | TC-004, TC-012, TC-031 | |
| Maximum Balance = $999,999.99 | TC-034 | |
| Menu Loop Until Exit | TC-025 | |
| State Persistence Within Session | TC-026, TC-039 | |

---

## Regression Test Suite (Priority Order)

### P1 - Critical (Must Pass)
- TC-001: Initial balance validation
- TC-003: Credit functionality
- TC-008: Debit with sufficient funds
- TC-009: Overdraft protection
- TC-037: Negative balance prevention
- TC-020: Exit functionality

### P2 - High (Should Pass)
- TC-010: Boundary condition (exact balance debit)
- TC-015: Mixed credit/debit operations
- TC-026: Balance persistence
- TC-029: Credit operation completion
- TC-030: Debit operation completion

### P3 - Medium (Nice to Have)
- TC-004: Decimal precision in credits
- TC-005: Large amount credits
- TC-012: Decimal precision in debits
- TC-038: Consistency of balance reads
- TC-031: Balance display format

### P4 - Low (Edge Cases)
- TC-006: Zero amount credit
- TC-011: Debit from zero balance
- TC-034: Maximum balance testing

---

## Known Issues / Limitations

1. **No Input Validation:** Non-numeric inputs may cause runtime errors
2. **No Transaction Log:** Transactions not recorded for audit trail
3. **Single Account:** No account identification capability
4. **Non-Persistent Storage:** Balance resets to $1,000.00 on program restart
5. **No Concurrent Access:** Not designed for multi-user scenarios

---

## Test Execution Notes

- **Each test case should start with a fresh program execution**
- **After each failed debit attempt (TC-009, TC-011, etc.), verify balance is unchanged**
- **Verify error messages are displayed exactly as specified**
- **Test menu reappears correctly after each operation**
- **Note any variation in expected output for documentation**

---

## Approval Sign-Off

| Role | Name | Date | Signature |
|---|---|---|---|
| Test Plan Author | | | |
| Business Stakeholder | | | |
| QA Lead | | | |
| Development Lead | | | |

---

## Notes for Node.js Migration

When migrating this test plan to Node.js unit and integration tests:

1. **View Balance** → Mock/test balance retrieval endpoint
2. **Credit Operation** → Test POST request with amount, validate response
3. **Debit Operation** → Test POST request with amount, validate overdraft protection
4. **Error Handling** → Test error responses (400 Bad Request, 422 Unprocessable Entity, etc.)
5. **Validation** → Implement input validation (numeric, decimal places, positive values)
6. **Persistence** → Use database layer instead of in-memory storage
7. **State Management** → Use proper state management/session handling
8. **API Contracts** → Define clear request/response schemas (JSON)
9. **Logging** → Add transaction logging for audit trail
10. **Security** → Add authentication, authorization, rate limiting
