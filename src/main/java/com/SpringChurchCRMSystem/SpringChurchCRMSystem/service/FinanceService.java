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

            if (loggedInUser.getRole() != RoleType.CellAdmin && loggedInUser.getRole() != RoleType.SuperAdmin) {
                return ResponseEntity.ok("Status 6000");
            }

            // Validate category
            if (finance.getCategory() == null) {
                return ResponseEntity.ok("Status 3000"); // Category is required
            }

            // Automatically set transaction type based on category type
            if (finance.getCategory() instanceof IncomeCategory) {
                finance.setTransactionType("INCOME");
            } else if (finance.getCategory() instanceof ExpenseCategory) {
                finance.setTransactionType("EXPENSE");
            } else {
                return ResponseEntity.ok("Status 3000"); // Invalid category type
            }

            // Validate category existence based on the determined transaction type
            if (finance.getTransactionType().equals("INCOME")) {
                IncomeCategory incomeCategory = (IncomeCategory) finance.getCategory();
                if (!incomeCategoryRepository.existsById(incomeCategory.getIncomeCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Income category not found
                }
            } else if (finance.getTransactionType().equals("EXPENSE")) {
                ExpenseCategory expenseCategory = (ExpenseCategory) finance.getCategory();
                if (!expenseCategoryRepository.existsById(expenseCategory.getExpenseCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Expense category not found
                }
            }

            // Validate amount
            if (finance.getAmount() <= 0) {
                return ResponseEntity.ok("Status 3000"); // Invalid amount
            }

            // Validate transaction date (cannot be in the future)
            if (finance.getTransactionDate() == null || finance.getTransactionDate().isAfter(LocalDate.now())) {
                return ResponseEntity.ok("Status 3000"); // Invalid transaction date
            }

            // Set creation date if not provided
            if (finance.getTransactionDate() == null) {
                finance.setTransactionDate(LocalDate.now());
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

            // Update fields if provided
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

            // Update transaction type and category if provided
            if (updatedData.getTransactionType() != null) {
                String newTransactionType = updatedData.getTransactionType();
                if (!newTransactionType.equals("INCOME") && !newTransactionType.equals("EXPENSE")) {
                    return ResponseEntity.ok("Status 3000"); // Invalid transaction type
                }
                finance.setTransactionType(newTransactionType);

                // If category is also being updated, validate it
                if (updatedData.getCategory() != null) {
                    if (newTransactionType.equals("INCOME") && !(updatedData.getCategory() instanceof IncomeCategory)) {
                        return ResponseEntity.ok("Status 3000"); // Category type mismatch
                    }
                    if (newTransactionType.equals("EXPENSE")
                            && !(updatedData.getCategory() instanceof ExpenseCategory)) {
                        return ResponseEntity.ok("Status 3000"); // Category type mismatch
                    }
                    finance.setCategory(updatedData.getCategory());
                }
            } else if (updatedData.getCategory() != null) {
                // Only category is being updated, validate against current transaction type
                if (finance.getTransactionType().equals("INCOME")
                        && !(updatedData.getCategory() instanceof IncomeCategory)) {
                    return ResponseEntity.ok("Status 3000"); // Category type mismatch
                }
                if (finance.getTransactionType().equals("EXPENSE")
                        && !(updatedData.getCategory() instanceof ExpenseCategory)) {
                    return ResponseEntity.ok("Status 3000"); // Category type mismatch
                }
                finance.setCategory(updatedData.getCategory());
            }

            // Validate category existence
            if (finance.getCategory() instanceof IncomeCategory) {
                IncomeCategory incomeCategory = (IncomeCategory) finance.getCategory();
                if (!incomeCategoryRepository.existsById(incomeCategory.getIncomeCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Income category not found
                }
            } else if (finance.getCategory() instanceof ExpenseCategory) {
                ExpenseCategory expenseCategory = (ExpenseCategory) finance.getCategory();
                if (!expenseCategoryRepository.existsById(expenseCategory.getExpenseCategoryId())) {
                    return ResponseEntity.ok("Status 3000"); // Expense category not found
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

    // Finance stats
    public FinanceStatsDTO getScopedFinanceStats(String userId) {
        User loggedInUser = userRepository.findByUserId(userId);
        if (loggedInUser == null) {
            return new FinanceStatsDTO(0, 0, 0);
        }

        double totalIncome;
        double totalExpenses;
        double currentBalance;

        if (loggedInUser.getRole() == RoleType.SuperAdmin) {
            totalIncome = financeRepository.sumAmountByTransactionType("INCOME");
            totalExpenses = financeRepository.sumAmountByTransactionType("EXPENSE");
        } else {
            // For scoped users, you might need to implement level-based filtering
            totalIncome = financeRepository.sumAmountByTransactionType("INCOME");
            totalExpenses = financeRepository.sumAmountByTransactionType("EXPENSE");
        }

        // Handle null values from repository
        totalIncome = totalIncome > 0 ? totalIncome : 0;
        totalExpenses = totalExpenses > 0 ? totalExpenses : 0;
        currentBalance = totalIncome - totalExpenses;

        return new FinanceStatsDTO(totalIncome, totalExpenses, currentBalance);
    }

}