# US 7.2.10 - As an Admin, I want to add new Types of rooms.

## 1. Context

This US creates the functionality to add new types of rooms

## 2. Requirements

**7.2.10** As an Admin, I want to add new Types of rooms, so that I can reflect on the available medical procedures in the system.

### 2.1. Acceptance Criteria

N/A

### 2.2. User Story Dependencies

N/A

## 3. Analysis

The admin will input the name of the type of room to add in the system.

The room type is composed by:

- Internal Code
- Designation
- Description (this field is optional)
- Sultability for surgeries (Yes or No)

The internal code is a 8 character long string constitued only by letters, numbers, and dashes. The internal code is unique and must not have spaces.

The designation is a free text with the maximum of 100 characters.

The room type will be a category to select the room when create a operation room in the system.

## 4. Design

### 4.1. Level 1

![L1](L1/Process_View.svg)

### 4.2. Level 2

![L2](L2/Process_View.svg)

### 4.3. Level 3

#### 4.3.1. Masters Data

![L3_MD](L3/MastersData/Process_View.svg)

#### 4.3.2. SPA

![L3_MD](L3/SPA/Process_View.svg)

### 4.4. Architeture used

- **Onion**: The Onion Architecture emphasizes a clear separation of concerns by organizing code into concentric layers, with the core domain at the center and external dependencies on the outer layers.

### 4.5. Patterns used

#### 4.5.1. Masters Data

- **Repository Pattern**: Used to abstract the data access layer, providing a collection-like interface for accessing domain objects.
- **Unit of Work Pattern**: Maintains a list of objects affected by a business transaction and coordinates the writing out of changes.
- **DTO (Data Transfer Object) Pattern**: Used to transfer data between software application subsystems.

#### 4.5.2. SPA

- **MVVM (Model-View-ViewModel) Pattern**: Separates the development of the graphical user interface from the business logic or back-end logic.
- **Service Pattern**: Encapsulates the business logic, making it reusable and easier to test.
- **Repository Pattern**: Used to manage data operations and abstract the data access logic.

## 5. Tests Plan

### 5.1. Unit Tests

- **Test 1**: Verify that a new room type can be added with valid data.
- **Test 2**: Verify that adding a room type with an existing internal code fails.
- **Test 3**: Verify that adding a room type with an invalid internal code fails.
- **Test 4**: Verify that adding a room type with a designation longer than 100 characters fails.
- **Test 5**: Verify that adding a room type without a description succeeds.
- **Test 6**: Verify that adding a room type with suitability for surgeries set to "Yes" or "No" succeeds.

### 5.2. Integration Tests

- **Test 1**: Verify that the room type is correctly saved in the database.
- **Test 2**: Verify that the room type can be retrieved from the database.
- **Test 3**: Verify that the room type can be used when creating an operation room.

### 5.3. End-to-End Tests

- **Test 1**: Verify that an admin can navigate to the add room type page and successfully add a new room type.
- **Test 2**: Verify that the system displays an error message when trying to add a room type with an existing internal code.
- **Test 3**: Verify that the system displays an error message when trying to add a room type with invalid data.