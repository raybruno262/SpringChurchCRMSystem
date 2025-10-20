package com.SpringChurchCRMSystem.SpringChurchCRMSystem.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.ResponseEntity;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.ExpenseCategory;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.ExpenseCategoryService;

@RestController
@RequestMapping("/api/expenseCategory")
public class ExpenseCategoryController {
    @Autowired
    private ExpenseCategoryService expenseCategoryService;

    // save expense category
    @PostMapping("/createExpenseCategory")
    public ResponseEntity<String> createExpenseCategory(@RequestBody ExpenseCategory expenseCategory) {

        return expenseCategoryService.createExpenseCategory(expenseCategory);

    }

    // update income category
    @PutMapping("/updateExpenseCategory/{expenseCategoryId}")
    public ResponseEntity<String> updateExpenseCategory(@PathVariable String expenseCategoryId,
            @RequestBody ExpenseCategory newExpenseCategory) {

        return expenseCategoryService.updateExpenseCategory(expenseCategoryId, newExpenseCategory);

    }

    // get all expense category
    @GetMapping("/allExpenseCategories")
    public List<ExpenseCategory> allExpenseCategories() {

        return expenseCategoryService.getAllExpenseCategories();

    }

    // Expense categories counts
    @GetMapping("/count")
    public long getTotalExpenseCategoryCount() {

        return expenseCategoryService.getTotalExpenseCategoryCount();

    }

    // get paginated expense categories
    @GetMapping("/getPaginatedExpenseCategories")
    public Page<ExpenseCategory> getPaginatedDepartments(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return expenseCategoryService.getPaginatedExpenseCategories(page, size);

    }

}
