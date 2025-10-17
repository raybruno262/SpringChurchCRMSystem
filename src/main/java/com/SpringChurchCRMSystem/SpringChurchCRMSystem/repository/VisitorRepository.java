package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Visitor;

public interface VisitorRepository extends MongoRepository<Visitor, String> {

    Page<Visitor> findAll(Pageable pageable);

    Page<Visitor> findByLevelIn(List<Level> levels, Pageable pageable);

    List<Visitor> findByLevelIn(List<Level> levels);

    long countByLevelIn(List<Level> levels);

    long countByStatus(String status);

    long countByStatusAndLevelIn(String status, List<Level> levels);
}
