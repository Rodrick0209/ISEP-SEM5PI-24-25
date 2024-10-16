using System;
using Xunit;
using DDDSample1.Domain.Patient;

namespace UnitTest.Domain.Patient
{
    public class MedicalRecordNumberGeneratorTest
    {
        [Fact]
        public void GenerateMedicalRecordNumber_ShouldReturnValidMedicalRecordNumber()
        {
            // Arrange
            var generator = new MedicalRecordNumberGenerator();
            string expectedYearMonth = DateTime.Now.ToString("yyyyMM");

            // Act
            var result = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber();

            // Assert
            Assert.NotNull(result);
            Assert.StartsWith(expectedYearMonth, result._medicalRecordNumber);
            Assert.True(result._medicalRecordNumber.Length > expectedYearMonth.Length);
        }

        [Fact]
        public void GenerateMedicalRecordNumber_ShouldIncrementSequentialNumber()
        {
            // Arrange
            var generator = new MedicalRecordNumberGenerator();
            string expectedYearMonth = DateTime.Now.ToString("yyyyMM");

            // Act
            var result1 = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber();
            var result2 = MedicalRecordNumberGenerator.GenerateMedicalRecordNumber();

            // Assert
            Assert.NotEqual(result1._medicalRecordNumber, result2._medicalRecordNumber);
            Assert.True(int.Parse(result2._medicalRecordNumber.Substring(expectedYearMonth.Length)) > int.Parse(result1._medicalRecordNumber.Substring(expectedYearMonth.Length)));
        }
    }
}