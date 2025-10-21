package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Equipment;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;

public interface EquipmentRepository extends MongoRepository<Equipment, String> {

    // For scoped access
    List<Equipment> findByLevelIn(List<Level> levels);

    Page<Equipment> findByLevelIn(List<Level> levels, Pageable pageable);

    // Counting methods for statistics
    long countByCondition(String condition);

    long countByLevelIn(List<Level> levels);

    long countByConditionAndLevelIn(String condition, List<Level> levels);
}
