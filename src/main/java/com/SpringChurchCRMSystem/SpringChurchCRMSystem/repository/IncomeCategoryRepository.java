package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.IncomeCategory;

@Repository
public interface IncomeCategoryRepository extends MongoRepository<IncomeCategory, String> {
    // find all income categories
    Page<IncomeCategory> findAll(Pageable pageable);

    // find income category by name
    Optional<IncomeCategory> findByNameIgnoreCase(String name);

    // check if income category exists by id
    boolean existsByIncomeCategoryId(String incomeCategoryId);

}
