package com.SpringChurchCRMSystem.SpringChurchCRMSystem;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

import java.util.List;

// Marks this class as a configuration class for Spring
@Configuration
public class SecurityConfig {

    // Defines the main security filter chain bean
    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                // Disables CSRF protection (useful for stateless APIs or testing)
                .csrf(csrf -> csrf.disable())

                // Enables CORS and sets the configuration source
                .cors(cors -> cors.configurationSource(corsConfigurationSource()))

                // Allows all incoming HTTP requests without authentication
                .authorizeHttpRequests(auth -> auth
                        .anyRequest().permitAll())

                // Disables the default login form
                .formLogin(form -> form.disable())

                // Disables HTTP Basic authentication
                .httpBasic(basic -> basic.disable());

        // Builds and returns the configured security filter chain
        return http.build();
    }

    // Defines a CORS filter bean to allow cross-origin requests
    @Bean
    public CorsFilter corsFilter() {
        // Creates a source object to map URL patterns to CORS configurations
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();

        // Allows credentials (cookies, authorization headers, etc.) in cross-origin
        // requests
        config.setAllowCredentials(true);

        // Allows requests from any origin (you can restrict this in production)
        config.setAllowedOriginPatterns(List.of("*")); // Allow all origins

        // Allows all headers in incoming requests
        config.setAllowedHeaders(List.of("*")); // Allow all headers

        // Allows common HTTP methods (GET, POST, PUT, DELETE, OPTIONS)
        config.setAllowedMethods(List.of("GET", "POST", "PUT", "DELETE", "OPTIONS")); // Allow all methods

        // Applies this CORS configuration to all URL paths
        source.registerCorsConfiguration("/**", config);

        // Returns the configured CORS filter
        return new CorsFilter(source);
    }

    // Provides the CORS configuration source used by Spring Security
    private UrlBasedCorsConfigurationSource corsConfigurationSource() {
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();

        // Allows credentials in requests
        config.setAllowCredentials(true);

        // Allows requests from any origin
        config.setAllowedOriginPatterns(List.of("*"));

        // Allows all headers
        config.setAllowedHeaders(List.of("*"));

        // Allows all HTTP methods
        config.setAllowedMethods(List.of("*"));

        // Applies this configuration to all endpoints
        source.registerCorsConfiguration("/**", config);

        // Returns the source object
        return source;
    }
}
