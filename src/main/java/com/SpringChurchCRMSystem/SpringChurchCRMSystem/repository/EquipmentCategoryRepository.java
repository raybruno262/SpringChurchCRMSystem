package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.EquipmentCategory;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.IncomeCategory;

@Repository
public interface EquipmentCategoryRepository extends MongoRepository<EquipmentCategory, String> {
    // find all equipment categories
    Page<EquipmentCategory> findAll(Pageable pageable);

    // find equipment category by name
    Optional<IncomeCategory> findByNameIgnoreCase(String name);

    // check if equipment category exists by id
    boolean existsByEquipmentCategoryId(String equipmentCategoryId);

}
