package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.time.LocalDate;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;

import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.Dto.FinanceStatsDTO;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.ExpenseCategory;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Finance;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.IncomeCategory;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.RoleType;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.ExpenseCategoryRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.FinanceRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.IncomeCategoryRepository;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.UserRepository;

@Service
public class FinanceService {

    @Autowired
    private FinanceRepository financeRepository;

    @Autowired
    private IncomeCategoryRepository incomeCategoryRepository;

    @Autowired
    private ExpenseCategoryRepository expenseCategoryRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private LevelService levelService;

    public ResponseEntity<String> createFinance(Finance finance, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not authorized
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // Validate transaction type
            if (finance.getTransactionType() == null ||
                    (!finance.getTransactionType().equals("INCOME")
                            && !finance.getTransactionType().equals("EXPENSE"))) {
                return ResponseEntity.ok("Status 3000"); // Invalid transaction type
            }

            // Validate category based on transaction type
            if (finance.getTransactionType().equals("INCOME")) {
                // Must have income category, not expense category
                if (finance.getIncomeCategory() == null || finance.getIncomeCategory().getIncomeCategoryId() == null) {
                    return ResponseEntity.ok("Status 3000"); // Income category required
                }
                if (!incomeCategoryRepository.existsById(finance.getIncomeCategory().getIncomeCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Income category not found
                }
                // Ensure expense category is null for income transactions
                finance.setExpenseCategory(null);
            } else if (finance.getTransactionType().equals("EXPENSE")) {
                // Must have expense category, not income category
                if (finance.getExpenseCategory() == null
                        || finance.getExpenseCategory().getExpenseCategoryId() == null) {
                    return ResponseEntity.ok("Status 3000"); // Expense category required
                }
                if (!expenseCategoryRepository.existsById(finance.getExpenseCategory().getExpenseCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Expense category not found
                }
                // Ensure income category is null for expense transactions
                finance.setIncomeCategory(null);
            }

            // Validate amount
            if (finance.getAmount() <= 0) {
                return ResponseEntity.ok("Status 3000"); // Invalid amount
            }

            // Validate transaction date
            if (finance.getTransactionDate() == null) {
                finance.setTransactionDate(LocalDate.now()); // Set current date if not provided
            } else if (finance.getTransactionDate().isAfter(LocalDate.now())) {
                return ResponseEntity.ok("Status 3000"); // Cannot have future transaction date
            }

            // Validate level
            if (finance.getLevel() == null || finance.getLevel().getLevelId() == null) {
                return ResponseEntity.ok("Status 3000"); // Level is required
            }

            // Validate description
            if (finance.getDescription() == null || finance.getDescription().trim().isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Description is required
            }

            financeRepository.save(finance);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    public Finance getFinanceById(String financeId) {
        return financeRepository.findById(financeId).orElse(null);
    }

    // Find all finance records
    public List<Finance> getAllFinanceRecords() {
        return financeRepository.findAll(Sort.by(Sort.Direction.DESC, "transactionDate"));
    }

    // Find all paginated finance records
    public Page<Finance> getPaginatedFinanceRecords(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "transactionDate"));
        return financeRepository.findAll(pageable);
    }

    // Update finance record
    public ResponseEntity<String> updateFinance(String financeId, Finance updatedData, String userId) {
        try {
            User loggedInUser = userRepository.findByUserId(userId);

            // No user logged in
            if (loggedInUser == null) {
                return ResponseEntity.ok("Status 4000");
            }

            // Not authorized
            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            Optional<Finance> financeOpt = financeRepository.findById(financeId);
            if (financeOpt.isEmpty()) {
                return ResponseEntity.ok("Status 3000"); // Finance record not found
            }

            Finance finance = financeOpt.get();


            if (updatedData.getTransactionDate() != null) {
                // Validate transaction date
                if (updatedData.getTransactionDate().isAfter(LocalDate.now())) {
                    return ResponseEntity.ok("Status 3000"); // Invalid transaction date
                }
                finance.setTransactionDate(updatedData.getTransactionDate());
            }

            if (updatedData.getAmount() > 0) {
                finance.setAmount(updatedData.getAmount());
            }

            if (updatedData.getDescription() != null) {
                finance.setDescription(updatedData.getDescription());
            }

            // Update level if provided
            if (updatedData.getLevel() != null) {
                finance.setLevel(updatedData.getLevel());
            }

            // Update transaction type if provided
            if (updatedData.getTransactionType() != null) {
                String newTransactionType = updatedData.getTransactionType();
                if (!newTransactionType.equals("INCOME") && !newTransactionType.equals("EXPENSE")) {
                    return ResponseEntity.ok("Status 3000"); // Invalid transaction type
                }
                finance.setTransactionType(newTransactionType);

                // Clear opposite category when transaction type changes
                if (newTransactionType.equals("INCOME")) {
                    finance.setExpenseCategory(null);
                } else if (newTransactionType.equals("EXPENSE")) {
                    finance.setIncomeCategory(null);
                }
            }

            // Update income category if provided and transaction type is INCOME
            if (updatedData.getIncomeCategory() != null) {
                if (finance.getTransactionType().equals("INCOME")) {
                    // Validate income category
                    if (updatedData.getIncomeCategory().getIncomeCategoryId() == null) {
                        return ResponseEntity.ok("Status 3000"); // Income category ID required
                    }
                    if (!incomeCategoryRepository.existsById(updatedData.getIncomeCategory().getIncomeCategoryId())) {
                        return ResponseEntity.ok("Status 3000"); // Income category not found
                    }
                    finance.setIncomeCategory(updatedData.getIncomeCategory());
                    // Ensure expense category is null
                    finance.setExpenseCategory(null);
                } else {
                    return ResponseEntity.ok("Status 3000"); // Cannot set income category for expense transaction
                }
            }

            // Update expense category if provided and transaction type is EXPENSE
            if (updatedData.getExpenseCategory() != null) {
                if (finance.getTransactionType().equals("EXPENSE")) {
                    // Validate expense category
                    if (updatedData.getExpenseCategory().getExpenseCategoryId() == null) {
                        return ResponseEntity.ok("Status 3000"); // Expense category ID required
                    }
                    if (!expenseCategoryRepository
                            .existsById(updatedData.getExpenseCategory().getExpenseCategoryId())) {
                        return ResponseEntity.ok("Status 3000"); // Expense category not found
                    }
                    finance.setExpenseCategory(updatedData.getExpenseCategory());
                    // Ensure income category is null
                    finance.setIncomeCategory(null);
                } else {
                    return ResponseEntity.ok("Status 3000"); // Cannot set expense category for income transaction
                }
            }

            financeRepository.save(finance);
            return ResponseEntity.ok("Status 1000"); // Success

        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.ok("Status 2000"); // Internal server error
        }
    }

    // Get scoped finance records (based on user's level/role)
    public Page<Finance> getScopedPaginatedFinanceRecords(int page, int size, String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return Page.empty();
        }

        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "transactionDate"));

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            return financeRepository.findAll(pageable);
        }

        List<Level> scopedLevels = levelService.getAllCellsUnder(loggedInUser.getLevel());

        if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                !scopedLevels.contains(loggedInUser.getLevel())) {
            scopedLevels.add(loggedInUser.getLevel());
        }

        return financeRepository.findByLevelIn(scopedLevels, pageable);
    }

    // In FinanceService - temporary manual calculation
    public FinanceStatsDTO getScopedFinanceStats(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return new FinanceStatsDTO(0, 0, 0);
        }

        List<Finance> financeRecords;

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            financeRecords = financeRepository.findAll();
        } else {
            List<Level> scopedCells = levelService.getAllCellsUnder(loggedInUser.getLevel());
            if (loggedInUser.getLevel().getLevelType() == LevelType.CELL &&
                    !scopedCells.contains(loggedInUser.getLevel())) {
                scopedCells.add(loggedInUser.getLevel());
            }
            financeRecords = financeRepository.findByLevelIn(scopedCells);
        }

        // Calculate sums manually from the list
        double totalIncome = financeRecords.stream()
                .filter(f -> f != null && "INCOME".equals(f.getTransactionType()))
                .mapToDouble(Finance::getAmount)
                .sum();

        double totalExpenses = financeRecords.stream()
                .filter(f -> f != null && "EXPENSE".equals(f.getTransactionType()))
                .mapToDouble(Finance::getAmount)
                .sum();

        double currentBalance = totalIncome - totalExpenses;

        return new FinanceStatsDTO(totalIncome, totalExpenses, currentBalance);
    }

}