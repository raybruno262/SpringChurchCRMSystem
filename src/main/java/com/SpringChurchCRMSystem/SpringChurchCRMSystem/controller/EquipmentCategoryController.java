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

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.EquipmentCategory;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.service.EquipmentCategoryService;

@RestController
@RequestMapping("/api/equipmentCategory")
public class EquipmentCategoryController {
    @Autowired
    private EquipmentCategoryService equipmentCategoryService;

    // save income category
    @PostMapping("/createEquipmentCategory")
    public ResponseEntity<String> createIncomeCategory(@RequestBody EquipmentCategory equipmentCategory) {

        return equipmentCategoryService.createEquipmentCategory(equipmentCategory);

    }

    // update equipment category
    @PutMapping("/updateEquipmentCategory/{equipmentCategoryId}")
    public ResponseEntity<String> updateIncomeCategory(@PathVariable String equipmentCategoryId,
            @RequestBody EquipmentCategory newEquipmentCategory) {

        return equipmentCategoryService.updateEquipmentCategory(equipmentCategoryId, newEquipmentCategory);

    }

    // get all equipment category
    @GetMapping("/allEquipmentCategories")
    public List<EquipmentCategory> allEquipmentCategories() {

        return equipmentCategoryService.getAllEquipmentCategories();

    }

    // Equipment categories counts
    @GetMapping("/count")
    public long getTotalEquipmentCategoryCount() {

        return equipmentCategoryService.getTotalEquipmentCategoryCount();

    }

    // get paginated equipment categories
    @GetMapping("/getPaginatedEquipmentCategories")
    public Page<EquipmentCategory> getPaginatedDepartments(@RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "5") int size) {

        return equipmentCategoryService.getPaginatedIncomeCategories(page, size);

    }

}
