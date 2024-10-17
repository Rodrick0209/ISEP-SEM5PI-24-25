using System;
using Xunit;
using DDDSample1.Domain.Patient;

namespace UnitTest.Domain.Patient
{
    public class PatientDtoTest
    {
        [Fact]
        public void TestPatientDtoCreation()
        {
            // Arrange
            var id = Guid.NewGuid();
            var fullName = "John Doe";
            var dateOfBirth = "1990-01-01";
            var email = "john.doe@example.com";
            var phoneNumber = "1234567890";
            var medicalRecordNumber = "202410000001";
            var gender = "male";
            var emergencyContact = "0987654321";

            // Act
            var patientDto = new PatientDto(id, fullName, dateOfBirth, email, phoneNumber, medicalRecordNumber, gender, emergencyContact);

            // Assert
            Assert.Equal(id, patientDto.Id);
            Assert.Equal(fullName, patientDto.FullName);
            Assert.Equal(dateOfBirth, patientDto.DateOfBirth);
            Assert.Equal(email, patientDto.Email);
            Assert.Equal(phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Equal(gender, patientDto.gender);
            Assert.Equal(emergencyContact, patientDto.EmergencyContact);
        }

        [Fact]
        public void TestPatientDtoCreationWithoutEmergencyContact()
        {
            // Arrange
            var id = Guid.NewGuid();
            var fullName = "John Doe";
            var dateOfBirth = "1990-01-01";
            var email = "john.doe@example.com";
            var phoneNumber = "1234567890";
            var medicalRecordNumber = "202410000001";

            // Act
            var patientDto = new PatientDto(id, fullName, dateOfBirth, email, phoneNumber, medicalRecordNumber);

            // Assert
            Assert.Equal(id, patientDto.Id);
            Assert.Equal(fullName, patientDto.FullName);
            Assert.Equal(dateOfBirth, patientDto.DateOfBirth);
            Assert.Equal(email, patientDto.Email);
            Assert.Equal(phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Null(patientDto.EmergencyContact);
        }

        [Fact]
        public void TestPatientDtoCreationWithoutGender()
        {
            // Arrange
            var id = Guid.NewGuid();
            var fullName = "John Doe";
            var dateOfBirth = "1990-01-01";
            var email = "john.doe@example.com";
            var phoneNumber = "1234567890";
            var medicalRecordNumber = "202410000001";
            var emergencyContact = "0987654321";

            // Act
            var patientDto = new PatientDto(id, fullName, dateOfBirth, email, phoneNumber, medicalRecordNumber, null, emergencyContact);

            // Assert
            Assert.Equal(id, patientDto.Id);
            Assert.Equal(fullName, patientDto.FullName);
            Assert.Equal(dateOfBirth, patientDto.DateOfBirth);
            Assert.Equal(email, patientDto.Email);
            Assert.Equal(phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Equal(emergencyContact, patientDto.EmergencyContact);
            Assert.Null(patientDto.gender);
        }

        [Fact]
        public void TestPatientDtoCreationWithNullValues()
        {
            // Arrange
            var id = Guid.NewGuid();
            var fullName = "John Doe";
            var dateOfBirth = "1990-01-01";
            var email = "john.doe@example.com";
            var phoneNumber = "1234567890";
            var medicalRecordNumber = "202410000001";

            // Act
            var patientDto = new PatientDto(id, fullName, dateOfBirth, email, phoneNumber, medicalRecordNumber, null, null);

            // Assert
            Assert.Equal(id, patientDto.Id);
            Assert.Equal(fullName, patientDto.FullName);
            Assert.Equal(dateOfBirth, patientDto.DateOfBirth);
            Assert.Equal(email, patientDto.Email);
            Assert.Equal(phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Null(patientDto.gender);
            Assert.Null(patientDto.EmergencyContact);
        }
    }
}
