package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;
import org.springframework.stereotype.Repository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.User;

@Repository
public interface UserRepository extends MongoRepository<User, String> {

    Optional<User> findByEmail(String email);

    // Find all active users
    List<User> findByIsActiveTrue();

    // Find all inactive users
    List<User> findByIsActiveFalse();

    // Pagination
    Page<User> findAll(Pageable pageable);

    Page<User> findByIsActiveTrue(Pageable pageable);

    Page<User> findByIsActiveFalse(Pageable pageable);

}
