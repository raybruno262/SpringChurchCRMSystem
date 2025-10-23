package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;

import org.springframework.boot.autoconfigure.data.web.SpringDataWebProperties.Sort;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Level;
import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;

public interface MemberRepository extends MongoRepository<Member, String> {

    // find all paginated members
    Page<Member> findAll(Pageable pageable);

    Page<Member> findByLevel_LevelIdIn(List<String> levelIds, Pageable pageable);

    Page<Member> findByLevelIn(List<Level> levels, Pageable pageable);

    List<Member> findByLevelIn(List<Level> levels);

    // Find paginated members with status = "Active"
    Page<Member> findByStatus(String status, Pageable pageable);

    long countByStatus(String status);

    long countByLevelIn(List<Level> levels);

    long countByStatusAndLevelIn(String status, List<Level> levels);

}
