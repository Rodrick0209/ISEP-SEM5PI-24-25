using System;
using DDDSample1.Domain.Patient;
using DDDSample1.Domain.Utils;
using Xunit;

namespace UnitTest.Domain.Patient
{
    public class PatientMapperTest
    {
        [Fact]
        public void ToDto_ShouldMapPatientToPatientDto()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var emergencyContact = new EmergencyContact("0987654321");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, emergencyContact, medicalRecordNumber);

            // Act
            var patientDto = PatientMapper.ToDto(patient);

            // Assert
            Assert.Equal(patient.Id.AsGuid(), patientDto.Id);
            Assert.Equal(patient.FullName.fullName, patientDto.FullName);
            Assert.Equal(patient.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd"), patientDto.DateOfBirth);
            Assert.Equal(patient.Email.email, patientDto.Email);
            Assert.Equal(patient.PhoneNumber.phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(patient.MedicalRecordNumber._medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Equal(patient.EmergencyContact.emergencyContact, patientDto.EmergencyContact);
        }

        [Fact]
        public void ToDto_ShouldHandleNullEmergencyContact()
        {
            // Arrange
            var fullName = new FullName("John Doe");
            var dateOfBirth = new DateOfBirth(new DateTime(1990, 1, 1));
            var email = new Email("john.doe@example.com");
            var phoneNumber = new PhoneNumber("1234567890");
            var medicalRecordNumber = new MedicalRecordNumber("202410000001");
            var patient = new DDDSample1.Domain.Patient.Patient(fullName, dateOfBirth, email, phoneNumber, null, medicalRecordNumber);

            // Act
            var patientDto = PatientMapper.ToDto(patient);

            // Assert
            Assert.Equal(patient.Id.AsGuid(), patientDto.Id);
            Assert.Equal(patient.FullName.fullName, patientDto.FullName);
            Assert.Equal(patient.DateOfBirth.dateOfBirth.ToString("yyyy-MM-dd"), patientDto.DateOfBirth);
            Assert.Equal(patient.Email.email, patientDto.Email);
            Assert.Equal(patient.PhoneNumber.phoneNumber, patientDto.PhoneNumber);
            Assert.Equal(patient.MedicalRecordNumber._medicalRecordNumber, patientDto.MedicalRecordNumber);
            Assert.Null(patientDto.EmergencyContact);
        }
    }
}