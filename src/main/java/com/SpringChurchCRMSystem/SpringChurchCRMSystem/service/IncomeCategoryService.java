package com.SpringChurchCRMSystem.SpringChurchCRMSystem.service;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.IncomeCategory;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository.IncomeCategoryRepository;

@Service
public class IncomeCategoryService {
    @Autowired
    private IncomeCategoryRepository incomeCategoryRepository;

    // create new income Category
    public ResponseEntity<String> createIncomeCategory(IncomeCategory incomeCategory) {
        if (incomeCategoryRepository.findByNameIgnoreCase(incomeCategory.getName()).isPresent()) {
            return ResponseEntity.ok("Status 5000"); // income category exists
        } else {
            incomeCategory.setName(incomeCategory.getName().trim());

            incomeCategoryRepository.save(incomeCategory);
            return ResponseEntity.ok("Status 1000"); // Success
        }
    }

    // update income category
    public ResponseEntity<String> updateIncomeCategory(String incomeCategoryId, IncomeCategory newIncomeCategoryId) {
        Optional<IncomeCategory> optdep = incomeCategoryRepository.findById(incomeCategoryId);
        if (optdep.isEmpty()) {
            return ResponseEntity.ok("Status 3000");
        }
        IncomeCategory d = optdep.get();
        d.setIncomeCategoryId(incomeCategoryId);
        d.setName(newIncomeCategoryId.getName());
        incomeCategoryRepository.save(d);
        return ResponseEntity.ok("Status 1000"); // Success

    }

    // get all income categories
    public List<IncomeCategory> getAllIncomeCategories() {
        return incomeCategoryRepository.findAll(Sort.by(Sort.Direction.DESC, "incomeCategoryId"));

    }

    // income categories count
    public long getTotalIncomeCategoryCount() {
        return incomeCategoryRepository.count();
    }

    // get all paginated income categories
    public Page<IncomeCategory> getPaginatedIncomeCategories(int page, int size) {
        PageRequest pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.DESC, "incomeCategoryId"));
        return incomeCategoryRepository.findAll(pageable);

    }

}
