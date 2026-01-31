/**
 * Accounting Application - Node.js Port of COBOL System
 * 
 * This application replicates the functionality of the original three COBOL programs:
 * - main.cob: MainProgram (UI and menu system)
 * - operations.cob: Operations (business logic)
 * - data.cob: DataProgram (data access layer)
 * 
 * Architecture: Three-tier model with separation of concerns
 * - Presentation Layer: Menu and user interaction
 * - Business Logic Layer: Transaction processing
 * - Data Layer: Balance storage and retrieval
 */

const readline = require('readline');

// ============================================================================
// DATA LAYER - Equivalent to data.cob (DataProgram)
// ============================================================================

class DataLayer {
  constructor() {
    // STORAGE-BALANCE from data.cob - Initial balance: 1000.00
    this.storageBalance = 1000.00;
  }

  /**
   * READ operation - Equivalent to 'READ' in data.cob
   * Retrieves current balance from storage
   * @returns {number} Current balance
   */
  read() {
    return this.storageBalance;
  }

  /**
   * WRITE operation - Equivalent to 'WRITE' in data.cob
   * Updates the stored balance
   * @param {number} balance - New balance to store
   */
  write(balance) {
    this.storageBalance = balance;
  }

  /**
   * Reset balance to initial value (for testing)
   */
  reset() {
    this.storageBalance = 1000.00;
  }
}

// ============================================================================
// BUSINESS LOGIC LAYER - Equivalent to operations.cob (Operations program)
// ============================================================================

class OperationsLayer {
  constructor(dataLayer) {
    this.dataLayer = dataLayer;
  }

  /**
   * TOTAL operation - View current balance
   * Equivalent to OPERATION-TYPE = 'TOTAL ' in operations.cob
   * @returns {object} Object with balance and message
   */
  viewBalance() {
    const balance = this.dataLayer.read();
    return {
      success: true,
      balance: balance,
      message: `Current balance: $${balance.toFixed(2)}`
    };
  }

  /**
   * CREDIT operation - Add funds to account
   * Equivalent to OPERATION-TYPE = 'CREDIT' in operations.cob
   * 
   * Business Logic:
   * 1. Read current balance
   * 2. Add credit amount
   * 3. Write updated balance
   * 4. Return new balance
   * 
   * @param {number} amount - Credit amount
   * @returns {object} Result object with success status and message
   */
  creditAccount(amount) {
    // Validate amount
    if (typeof amount !== 'number' || amount < 0) {
      return {
        success: false,
        balance: this.dataLayer.read(),
        message: 'Invalid credit amount. Please enter a positive number.'
      };
    }

    // Read current balance
    let finalBalance = this.dataLayer.read();

    // Add amount to balance
    finalBalance += amount;

    // Check maximum balance constraint (PIC 9(6)V99 = 999999.99)
    if (finalBalance > 999999.99) {
      return {
        success: false,
        balance: this.dataLayer.read(),
        message: `Credit failed. New balance ($${finalBalance.toFixed(2)}) would exceed maximum allowed ($999,999.99).`
      };
    }

    // Write updated balance
    this.dataLayer.write(finalBalance);

    return {
      success: true,
      balance: finalBalance,
      message: `Amount credited. New balance: $${finalBalance.toFixed(2)}`
    };
  }

  /**
   * DEBIT operation - Withdraw funds from account
   * Equivalent to OPERATION-TYPE = 'DEBIT ' in operations.cob
   * 
   * Business Logic:
   * 1. Read current balance
   * 2. Validate sufficient funds available (CRITICAL BUSINESS RULE)
   * 3. If sufficient: subtract amount and write new balance
   * 4. If insufficient: display error, leave balance unchanged
   * 
   * CRITICAL BUSINESS RULE: No overdraft allowed
   * - "Insufficient funds for this debit" error message
   * - Balance must never go negative
   * - Transaction rejected if amount > available balance
   * 
   * @param {number} amount - Debit amount
   * @returns {object} Result object with success status and message
   */
  debitAccount(amount) {
    // Validate amount
    if (typeof amount !== 'number' || amount < 0) {
      return {
        success: false,
        balance: this.dataLayer.read(),
        message: 'Invalid debit amount. Please enter a positive number.'
      };
    }

    // Read current balance
    let finalBalance = this.dataLayer.read();

    // CRITICAL BUSINESS RULE: Check if sufficient funds available
    // This prevents overdraft and negative balances
    if (finalBalance < amount) {
      return {
        success: false,
        balance: finalBalance,
        message: 'Insufficient funds for this debit.'
      };
    }

    // Subtract amount from balance
    finalBalance -= amount;

    // Write updated balance
    this.dataLayer.write(finalBalance);

    return {
      success: true,
      balance: finalBalance,
      message: `Amount debited. New balance: $${finalBalance.toFixed(2)}`
    };
  }
}

// ============================================================================
// PRESENTATION LAYER - Equivalent to main.cob (MainProgram)
// ============================================================================

class AccountingApplication {
  constructor() {
    this.dataLayer = new DataLayer();
    this.operationsLayer = new OperationsLayer(this.dataLayer);
    this.continueFlag = true;

    // Setup readline interface for user input
    this.rl = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });
  }

  /**
   * Display main menu
   * Equivalent to the DISPLAY statements in main.cob
   */
  displayMenu() {
    console.log('\n--------------------------------');
    console.log('Account Management System');
    console.log('1. View Balance');
    console.log('2. Credit Account');
    console.log('3. Debit Account');
    console.log('4. Exit');
    console.log('--------------------------------');
  }

  /**
   * Get user menu choice
   * Equivalent to ACCEPT USER-CHOICE in main.cob
   */
  getUserChoice() {
    return new Promise((resolve) => {
      this.rl.question('Enter your choice (1-4): ', (answer) => {
        resolve(parseInt(answer, 10));
      });
    });
  }

  /**
   * Get amount input from user
   */
  getAmountInput(prompt) {
    return new Promise((resolve) => {
      this.rl.question(prompt, (answer) => {
        resolve(parseFloat(answer));
      });
    });
  }

  /**
   * Handle menu choice 1 - View Balance
   * Calls operationsLayer.viewBalance()
   */
  async handleViewBalance() {
    const result = this.operationsLayer.viewBalance();
    console.log(`\n${result.message}`);
  }

  /**
   * Handle menu choice 2 - Credit Account
   * Calls operationsLayer.creditAccount(amount)
   */
  async handleCreditAccount() {
    const amount = await this.getAmountInput('Enter credit amount: $');
    const result = this.operationsLayer.creditAccount(amount);
    console.log(`\n${result.message}`);
  }

  /**
   * Handle menu choice 3 - Debit Account
   * Calls operationsLayer.debitAccount(amount)
   */
  async handleDebitAccount() {
    const amount = await this.getAmountInput('Enter debit amount: $');
    const result = this.operationsLayer.debitAccount(amount);
    console.log(`\n${result.message}`);
  }

  /**
   * Main program logic
   * Equivalent to MAIN-LOGIC in main.cob
   * 
   * PERFORM UNTIL CONTINUE-FLAG = 'NO'
   * - Display menu
   * - Accept user choice
   * - Evaluate choice and call appropriate operation
   * - Loop until choice 4 (Exit)
   */
  async run() {
    console.log('\n=== COBOL Account Management System (Node.js Port) ===\n');

    while (this.continueFlag) {
      this.displayMenu();
      const userChoice = await this.getUserChoice();

      switch (userChoice) {
        case 1:
          // WHEN 1: CALL 'Operations' USING 'TOTAL '
          await this.handleViewBalance();
          break;

        case 2:
          // WHEN 2: CALL 'Operations' USING 'CREDIT'
          await this.handleCreditAccount();
          break;

        case 3:
          // WHEN 3: CALL 'Operations' USING 'DEBIT '
          await this.handleDebitAccount();
          break;

        case 4:
          // WHEN 4: MOVE 'NO' TO CONTINUE-FLAG
          this.continueFlag = false;
          break;

        default:
          // WHEN OTHER: Invalid choice message
          console.log('Invalid choice, please select 1-4.');
      }
    }

    // DISPLAY "Exiting the program. Goodbye!"
    // STOP RUN
    console.log('\nExiting the program. Goodbye!');
    this.rl.close();
  }
}

// ============================================================================
// APPLICATION ENTRY POINT
// ============================================================================

// Only run if this is the main module
if (require.main === module) {
  const app = new AccountingApplication();
  app.run().catch((error) => {
    console.error('Application error:', error);
    process.exit(1);
  });
}

// Export classes for testing
module.exports = {
  DataLayer,
  OperationsLayer,
  AccountingApplication
};
