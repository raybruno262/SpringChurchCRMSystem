package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Department;

@Repository
public interface DepartmentRepository extends MongoRepository<Department, String> {
    Page<Department> findAll(Pageable pageable);

    Optional<Department> findByName(String name);

    boolean existsByDepartmentId(String departmentId);

}
