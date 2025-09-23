# Stage 1: Build the Spring Boot app
FROM maven:3.9.4-eclipse-temurin-17 AS builder
WORKDIR /build
COPY . .
RUN mvn clean package -DskipTests

# Stage 2: Run the app
FROM openjdk:17-jdk-slim
WORKDIR /app

# ✅ Correct reference to the named build stage
COPY --from=builder /build/target/*.jar app.jar