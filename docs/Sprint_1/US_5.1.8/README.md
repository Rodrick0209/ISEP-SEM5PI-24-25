# US 5.1.8 - As an Admin, I want to create a new patient profile

## 1. Context

This US creates the functionality to create a new patient profile.

## 2. Requirements

**5.1.8** As an Admin, I want to create a new patient profile, so that I can register their personal details and medical history.

### 2.1. Acceptance Criteria

- Admins can input patient details such as first name, last name, date of birth, contact information, and medical history.
- A unique patient ID (Medical Record Number) is generated upon profile creation.
- The system validates that the patient’s email and phone number are unique.
- The profile is stored securely in the system, and access is governed by role-based permissions.

### 2.2. User Story Dependencies

N/A

## 3. Analysis

To create a new patient profile, the admin must register:
- First Name
- Last Name
- Full Name
- Date of Birth
- Gender
- Contact Information (email, phone)
- Emergency Contact

When the content is registered, a **Medical Record Number** will be generated.

The patient's email and phone number needs to be **unique** in system.

The system should store the created profile **securely** in system.

### 3.1. Client-relevant questions

**Q:** It is specified that the admin can input some of the patient's information (name, date of birth, contact information, and medical history).
Do they also input the omitted information (gender, emergency contact and allergies/medical condition)?
Additionally, does the medical history that the admin inputs refer to the patient's medical record, or is it referring to the appointment history?

**R:** the admin can not input medical history nor allergies. they can however input gender and emergency contact