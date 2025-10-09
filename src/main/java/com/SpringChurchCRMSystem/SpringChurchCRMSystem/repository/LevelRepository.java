package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.LevelType;
import java.util.List;

public interface LevelRepository extends MongoRepository<Level, String> {
    // Find by name and type
    Optional<Level> findByNameAndLevelType(String name, LevelType levelType);

    // Pagination
    Page<Level> findAll(Pageable pageable);

    // Find children of a given parent
    List<Level> findByParent(Level parent);

    // Find all active levels
    List<Level> findByIsActiveTrue();

    // Find all inactive levels
    List<Level> findByIsActiveFalse();

    // get total level counts using level types
    int countByLevelType(String levelType);

}
