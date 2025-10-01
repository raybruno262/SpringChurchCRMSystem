package com.SpringChurchCRMSystem.SpringChurchCRMSystem.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.mongodb.repository.MongoRepository;

import com.SpringChurchCRMSystem.SpringChurchCRMSystem.model.Member;

public interface MemberRepository extends MongoRepository<Member, String> {
    boolean existsByEmail(String email);

    Page<Member> findAll(Pageable pageable);

    Page<Member> findByLevel_LevelIdIn(List<String> levelIds, Pageable pageable);

}
