package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.ExpenseCategory;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.ExpenseCategoryRepository;

@Service
public class ExpenseCategoryService {
    @Autowired
    private ExpenseCategoryRepository expenseCategoryRepository;

    // create new expense Category
    public ResponseEntity<String> createExpenseCategory(ExpenseCategory expenseCategory) {
        if (expenseCategoryRepository.findByNameIgnoreCase(expenseCategory.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // expense category exists
        } else {
            expenseCategory.setName(expenseCategory.getName().trim());

            expenseCategoryRepository.save(expenseCategory);
            return ResponseEntity.ok("Status 1000"); // Success
        }
    }

    // update expense category
    public ResponseEntity<String> updateExpenseCategory(String expenseCategoryId,
            ExpenseCategory newExpenseCategoryId) {
        Optional<ExpenseCategory> optdep = expenseCategoryRepository.findById(expenseCategoryId);
        if (optdep.isEmpty()) {
            return ResponseEntity.ok("Status 3000");
        }
             if (expenseCategoryRepository.findByNameIgnoreCase(newExpenseCategoryId.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // expense category exists
        } else {
        ExpenseCategory d = optdep.get();
        d.setExpenseCategoryId(expenseCategoryId);
        d.setName(newExpenseCategoryId.getName());
        expenseCategoryRepository.save(d);
        return ResponseEntity.ok("Status 1000"); // Success
        }
    }

    // get all expense categories
    public List<ExpenseCategory> getAllExpenseCategories() {
        return expenseCategoryRepository.findAll(Sort.by(Sort.Direction.DESC, "expenseCategoryId"));

    }

    // expense count
    public long getTotalExpenseCategoryCount() {
        return expenseCategoryRepository.count();
    }

    // get all paginated expense categories
    public Page<ExpenseCategory> getPaginatedExpenseCategories(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "expenseCategoryId"));
        return expenseCategoryRepository.findAll(pageable);

    }

}
