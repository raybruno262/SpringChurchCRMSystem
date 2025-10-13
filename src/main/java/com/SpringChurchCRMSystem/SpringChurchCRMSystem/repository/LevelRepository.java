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

    // find by leveltype
    List<Level> findByLevelType(LevelType levelType);

 

    // Find first level by level type (for finding existing headquarter)
    Optional<Level> findFirstByLevelType(LevelType levelType);

    // Find by name, level type and parent (for duplicate checking in hierarchy)
    Optional<Level> findByNameAndLevelTypeAndParent(String name, LevelType levelType, Level parent);

    // Find by level type and parent (for finding children of specific type)
    List<Level> findByLevelTypeAndParent(LevelType levelType, Level parent);

    // Find by name and parent (alternative duplicate check)
    Optional<Level> findByNameAndParent(String name, Level parent);
}