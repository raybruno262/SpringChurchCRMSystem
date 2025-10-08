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

    Page<Level> findByIsActiveTrue(Pageable pageable);

    Page<Level> findByIsActiveFalse(Pageable pageable);

    // Find children of a given parent
    List<Level> findByParent(Level parent);

    // Find all active levels
    List<Level> findByIsActiveTrue();

    // Find all inactive levels
    List<Level> findByIsActiveFalse();

    // Find active children of a given parent
    List<Level> findByParentAndIsActiveTrue();

    // Find inactive children of a given parent
    List<Level> findByParentAndIsActiveFalse();

    // get total level counts using level types
    int countByLevelType(String levelType);

}
