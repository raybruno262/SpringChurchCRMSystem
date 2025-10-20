package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.ExpenseCategory;

@Repository
public interface ExpenseCategoryRepository extends MongoRepository<ExpenseCategory, String> {
    // find all expense categories
    Page<ExpenseCategory> findAll(Pageable pageable);

    // find expense category by name
    Optional<ExpenseCategory> findByNameIgnoreCase(String name);

    // check if expense category exists by id
    boolean existsByExpenseCategoryId(String expenseCategoryId);

}
