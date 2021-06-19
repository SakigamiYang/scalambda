FROM openjdk:11-jre-slim-buster
WORKDIR /app
ADD build/libs/scalambda-1.0.0-all.jar /app
CMD ["java", "-jar", "/app/scalambda-1.0.0-all.jar"]
