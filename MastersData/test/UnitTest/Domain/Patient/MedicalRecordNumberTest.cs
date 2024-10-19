using System;
using DDDSample1.Domain.Patients;
using Xunit;

namespace UnitTest.Domain.Patient
{
    public class MedicalRecordNumberTest
    {
        [Fact]
        public void Should_Create_MedicalRecordNumber_When_Valid()
        {
            // Arrange
            string validMedicalRecordNumber = "202301000001";

            // Act
            var medicalRecordNumber = new MedicalRecordNumber(validMedicalRecordNumber);

            // Assert
            Assert.Equal(validMedicalRecordNumber, medicalRecordNumber._medicalRecordNumber);
        }

        [Fact]
        public void Should_Throw_Exception_When_MedicalRecordNumber_Is_NullOrWhiteSpace()
        {
            // Arrange
            string invalidMedicalRecordNumber = " ";

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidMedicalRecordNumber));
        }

        [Fact]
        public void Should_Throw_Exception_When_MedicalRecordNumber_Is_Not_12_Characters_Long()
        {
            // Arrange
            string invalidMedicalRecordNumber = "20230100001"; // 11 characters

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidMedicalRecordNumber));
        }

        [Fact]
        public void Should_Throw_Exception_When_Year_Is_Invalid()
        {
            // Arrange
            string invalidMedicalRecordNumber = "999901000001"; // Year 9999 is in the future

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidMedicalRecordNumber));
        }

        [Fact]
        public void Should_Throw_Exception_When_Month_Is_Invalid()
        {
            // Arrange
            string invalidMedicalRecordNumber = "202313000001"; // Month 13 is invalid

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidMedicalRecordNumber));
        }

        [Fact]
        public void Should_Throw_Exception_When_Sequential_Number_Is_Invalid()
        {
            // Arrange
            string invalidMedicalRecordNumber = "202301ABCDEF"; // Sequential number is not numeric

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new MedicalRecordNumber(invalidMedicalRecordNumber));
        }
    }
}