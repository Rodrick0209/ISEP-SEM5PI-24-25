# US5.1.30 As an Admin, I want to register new backoffice users (e.g., doctors, nurses, technicians, admins) via an out-of-band process, so that they can access the backoffice system with appropriate permissions.

## 1. Analysis

### Functional Requirements

The objective of this User Story (US) is to enable the Admin to register new backoffice users, such as doctors, nurses, and technicians, through an internal process. This ensures that only authorized personnel gain access, with roles assigned according to their duties.

### Business Rules

* **Admin-Only Registration**: Only Admins can register new users; self-registration is not permitted.
* **Role Assignment**: Admin assigns a specific role (e.g., Doctor, Nurse, Technician) to each user at the time of registration.
* **One-Time Setup Link**: Registered users receive an email with a one-time link to set their password and activate their account.
* **Strong Password Requirement**: Enforced password policies ensure strong security.
* **Verification Email**: Upon successful registration, a confirmation email verifies the user’s registration.

### Given Information to be Processed for Business Needs

The following information is required to process and register a new backoffice user:

- **ID**: A unique identifier for each user.
- **Name**: Full name of the user (e.g., Dr. John Doe).
- **Email**: User’s official email, used to send the one-time setup link and registration confirmation.
- **Role**: Role assigned to the user (e.g., Doctor, Nurse, Technician, Admin).
- **Status**: Account activation status (active/inactive).

This information will be processed and stored to ensure secure and controlled access to the backoffice system.

### Stakeholders

* **Admin**: Manages user registrations and role assignments.
* **Backoffice Users**: Access the system to perform their roles (e.g., doctors, nurses).
* **IT Security**: Ensures password policies and secure account setup.

### Preconditions

* The Admin must be authenticated in the system.
* The user's email must be unique and not already registered.

### Postconditions

* A new user account is created and activated, allowing the user to access the system based on their role.
* The user completes account setup by setting a secure password and activating their account through the one-time link.

---

## 2. Design

### Domain Model
![DM](DM/dm.png)

### High-Level Process View (L1)
![L1_Process_View](L1/Process_View.svg)

### Detailed Process View (L2)
![L2_Process_View](L2/Process_View.svg)

### Code Examples (L3)
![L3_Process_View](L3/Process_View.svg)

---

### Patterns Applied

* **GRASP (General Responsibility Assignment Software Patterns)**: Used to assign controllers for handling system events.
* **DTO (Data Transfer Object)**: Structured to manage the transfer of user registration data.
* **SOLID Principles**: Applied to ensure modular and maintainable code (Single Responsibility, Dependency Inversion).

## 3. Implementation

### Implementation Steps

1. **Database Schema Update**:
   - Add a `users` table with fields such as `id`, `name`, `email`, `role`.
   - Ensure `email` is unique.

2. **Backend Development**:
   - Create a `User` model to represent the user entity.
   - Implement a `UserService` to manage the business logic of registration, activation, and role assignment.
   - Develop a `UserController` to handle API endpoints.

3. **API Endpoints**:
   - `POST /register-user`: Endpoint for Admins to register a new user.
   - `POST /activate-account`: Endpoint to set up a password and activate the account.

4. **Validation Logic**:
   - Verify that the email is unique during registration.
   - Enforce strong password requirements on activation.

---

## 4. Testing

### 1. Business Rule Tests

#### 1.1 Admin-Only Access Test
- **Description**: Test if only authenticated Admins can register new users.
- **Scenario**: Attempt registration as a non-Admin user.
- **Expected Result**: The system should reject the request and prompt for admin credentials.

#### 1.2 Unique Email Validation Test
- **Description**: Test if the system prevents duplicate email registrations.
- **Scenario**: Attempt to register a user with an email that already exists.
- **Expected Result**: The system should reject the request and display an error message.

#### 1.3 Role Assignment Test
- **Description**: Test if roles are correctly assigned to new users.
- **Scenario**: Register a user and assign them a specific role.
- **Expected Result**: The user should be registered with the designated role.

---

### 2. Security Tests

#### 2.1 Password Complexity Test
- **Description**: Ensure passwords meet the security requirements.
- **Scenario**: Attempt activation with a password that does not meet the criteria.
- **Expected Result**: The system should reject the password and prompt for a stronger one.

#### 2.2 One-Time Link Expiration Test
- **Description**: Ensure that one-time setup links expire after 24 hours.
- **Scenario**: Try activating an account using an expired link.
- **Expected Result**: The system should reject the request and prompt for a new link.

---

### 3. Integration Tests

#### 3.1 Email Service Test
- **Description**: Verify that emails are sent correctly for activation and confirmation.
- **Scenario**: Register a new user and verify the receipt of both activation and confirmation emails.
- **Expected Result**: Both emails should be sent and correctly display setup information.

---

### 4. Error Handling and Feedback Tests

#### 4.1 Error Message Display Test
- **Description**: Ensure the system displays appropriate error messages for invalid inputs (e.g., duplicate email, weak password).
- **Scenario**: Submit invalid or duplicate information.
- **Expected Result**: The system should display clear error messages for each invalid input.

---

Admins can initiate secure user registration and assign roles, with users verifying and activating their accounts via email. This controlled registration process ensures that only authorized users access the system.
