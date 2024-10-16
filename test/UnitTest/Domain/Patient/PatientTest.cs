using System;
using Xunit;
using DDDSample1.Domain.Patient;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;

namespace UnitTest.Domain.Patient
{
    public class PatientTest
    {
        [Fact]
        public void TestPatientCreation()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");

            // Act
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            // Assert
            Assert.NotNull(patient.Id);
            Assert.Equal(fullName, patient.FullName);
            Assert.Equal(dateOfBirth, patient.DateOfBirth);
            Assert.Equal(email, patient.Email);
            Assert.Equal(phoneNumber, patient.PhoneNumber);
            Assert.Equal(emergencyContact, patient.EmergencyContact);
            Assert.Equal(medicalRecordNumber, patient.MedicalRecordNumber);
        }

        [Fact]
        public void TestChangeFullName()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            var newFullName = new FullName("John Smith");

            // Act
            patient.ChangeFullName(newFullName);

            // Assert
            Assert.Equal(newFullName, patient.FullName);
        }

        [Fact]
        public void TestChangeEmail()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            var newEmail = new Email("john.smith@example.com");

            // Act
            patient.ChangeEmail(newEmail);

            // Assert
            Assert.Equal(newEmail, patient.Email);
        }

        [Fact]
        public void TestChangePhoneNumber()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            var newPhoneNumber = new PhoneNumber("0987654321");

            // Act
            patient.ChangePhoneNumber(newPhoneNumber);

            // Assert
            Assert.Equal(newPhoneNumber, patient.PhoneNumber);
        }

        [Fact]
        public void TestChangeMedicalConditions()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            var medicalConditions = new MedicalConditions("Diabetes");

            // Act
            patient.ChangeMedicalConditions(medicalConditions);

            // Assert
            Assert.Equal(medicalConditions, patient.MedicalConditions);
        }
    }
}