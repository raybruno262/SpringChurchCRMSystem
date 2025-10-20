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

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.IncomeCategory;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.IncomeCategoryService;

@RestController
@RequestMapping("/api/incomeCategory")
public class IncomeCategoryController {
    @Autowired
    private IncomeCategoryService incomeCategoryService;

    // save income category
    @PostMapping("/createIncomeCategory")
    public ResponseEntity<String> createIncomeCategory(@RequestBody IncomeCategory incomeCategory) {

        return incomeCategoryService.createIncomeCategory(incomeCategory);

    }

    // update income category
    @PutMapping("/updateIncomeCategory/{incomeCategoryId}")
    public ResponseEntity<String> updateIncomeCategory(@PathVariable String incomeCategoryId,
            @RequestBody IncomeCategory newIncomeCategory) {

        return incomeCategoryService.updateIncomeCategory(incomeCategoryId, newIncomeCategory);

    }

    // get all income category
    @GetMapping("/allIncomeCategories")
    public List<IncomeCategory> allIncomeCategories() {

        return incomeCategoryService.getAllIncomeCategories();

    }

    // Income categories counts
    @GetMapping("/count")
    public long getTotalIncomeCategoryCount() {

        return incomeCategoryService.getTotalIncomeCategoryCount();

    }

    // get paginated income categories
    @GetMapping("/getPaginatedIncomeCategories")
    public Page<IncomeCategory> getPaginatedDepartments(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return incomeCategoryService.getPaginatedIncomeCategories(page, size);

    }

}
