using System;
using Xunit;
using DDDSample1.Domain.Patient;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;

namespace Tests.UnitTests.Domain.Patient
{
    public class PatientTest
    {
        [Fact]
        public void CreatePatient_ShouldCreatePatientWithValidData()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("123456789");
            var emergencyContact = new EmergencyContact("Jane Doe");
            var medicalRecordNumber = new MedicalRecordNumber("202310000012");

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
        public void ChangeFullName_ShouldUpdateFullName()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("123456789");
            var emergencyContact = new EmergencyContact("Jane Doe");
            var medicalRecordNumber = new MedicalRecordNumber("202310000012");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);
            var newFullName = new FullName("John Smith");

            // Act
            patient.ChangeFullName(newFullName);

            // Assert
            Assert.Equal(newFullName, patient.FullName);
        }

        [Fact]
        public void ChangeEmail_ShouldUpdateEmail()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("123456789");
            var emergencyContact = new EmergencyContact("987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202310000012");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);
            var newEmail = new Email("john.smith@example.com");

            // Act
            patient.ChangeEmail(newEmail);

            // Assert
            Assert.Equal(newEmail, patient.Email);
        }

        [Fact]
        public void ChangePhoneNumber_ShouldUpdatePhoneNumber()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("123456789");
            var emergencyContact = new EmergencyContact("987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202310000012");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);
            var newPhoneNumber = new PhoneNumber("987654321");

            // Act
            patient.ChangePhoneNumber(newPhoneNumber);

            // Assert
            Assert.Equal(newPhoneNumber, patient.PhoneNumber);
        }

        [Fact]
        public void ChangeMedicalConditions_ShouldUpdateMedicalConditions()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("123456789");
            var emergencyContact = new EmergencyContact("987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202310000012");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);
            var medicalConditions = new MedicalConditions("Diabetes");

            // Act
            patient.ChangeMedicalConditions(medicalConditions);

            // Assert
            Assert.Equal(medicalConditions, patient.MedicalConditions);
        }
    }
}