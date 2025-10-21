package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.EquipmentCategory;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.EquipmentCategoryRepository;

@Service
public class EquipmentCategoryService {
    @Autowired
    private EquipmentCategoryRepository equipmentCategoryRepository;

    // create new equipment Category
    public ResponseEntity<String> createEquipmentCategory(EquipmentCategory equipmentCategory) {
        if (equipmentCategoryRepository.findByNameIgnoreCase(equipmentCategory.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // equipment category exists
        } else {
            equipmentCategory.setName(equipmentCategory.getName().trim());

            equipmentCategoryRepository.save(equipmentCategory);
            return ResponseEntity.ok("Status 1000"); // Success
        }
    }

    // update equipment category
    public ResponseEntity<String> updateEquipmentCategory(String equipmentCategoryId,
            EquipmentCategory newEquipmentCategoryId) {
        Optional<EquipmentCategory> optdep = equipmentCategoryRepository.findById(equipmentCategoryId);
        if (optdep.isEmpty()) {
            return ResponseEntity.ok("Status 3000");
        }
        if (equipmentCategoryRepository.findByNameIgnoreCase(newEquipmentCategoryId.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // equipment category exists
        } else {
            EquipmentCategory d = optdep.get();
            d.setEquipmentCategoryId(equipmentCategoryId);
            d.setName(newEquipmentCategoryId.getName());
            equipmentCategoryRepository.save(d);
            return ResponseEntity.ok("Status 1000"); // Success

        }
    }

    // get all equipment categories
    public List<EquipmentCategory> getAllEquipmentCategories() {
        return equipmentCategoryRepository.findAll(Sort.by(Sort.Direction.DESC, "equipmentCategoryId"));

    }

    // equipment categories count
    public long getTotalEquipmentCategoryCount() {
        return equipmentCategoryRepository.count();
    }

    // get all paginated equipment categories
    public Page<EquipmentCategory> getPaginatedIncomeCategories(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "equipmentCategoryId"));
        return equipmentCategoryRepository.findAll(pageable);

    }

}
