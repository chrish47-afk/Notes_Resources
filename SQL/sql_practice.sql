-- SQL Practice Queries

-- Create a sample table
CREATE TABLE employees (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    department VARCHAR(50),
    age INT,
    salary DECIMAL(10, 2)
);

-- Insert data
INSERT INTO employees (name, department, age, salary) VALUES
('Alice', 'HR', 25, 50000),
('Bob', 'IT', 30, 60000),
('Charlie', 'Finance', 35, 75000),
('David', 'IT', 40, 80000),
('Emma', 'HR', 29, 55000);

-- Basic Queries
SELECT * FROM employees;
SELECT name, salary FROM employees WHERE salary > 60000;
SELECT department, AVG(salary) AS avg_salary FROM employees GROUP BY department;

-- Joins
SELECT e.name, d.department_name FROM employees e
JOIN departments d ON e.department = d.department_id;

-- Subqueries
SELECT name FROM employees WHERE salary > (SELECT AVG(salary) FROM employees);

-- Window Functions
SELECT name, salary, 
       RANK() OVER (ORDER BY salary DESC) AS salary_rank 
FROM employees;

-- Indexing for Performance
CREATE INDEX idx_salary ON employees(salary);

-- Query Optimization
EXPLAIN ANALYZE SELECT * FROM employees WHERE salary > 60000;
