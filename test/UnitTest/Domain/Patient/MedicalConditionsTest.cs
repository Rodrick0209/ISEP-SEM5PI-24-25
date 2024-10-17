using System;
using DDDSample1.Domain.Patient;
using Xunit;

namespace UnitTest.Domain.Patient
{
    public class MedicalConditionsTest
    {
        [Fact]
        public void Should_Create_MedicalConditions_When_Valid_String()
        {
            // Arrange
            string validMedicalConditions = "Diabetes";

            // Act
            var medicalConditions = new MedicalConditions(validMedicalConditions);

            // Assert
            Assert.Equal(validMedicalConditions, medicalConditions.medicalConditions);
        }

        [Fact]
        public void Should_Throw_ArgumentNullException_When_Null_Or_Empty_String()
        {
            // Arrange
            string invalidMedicalConditions = " ";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new MedicalConditions(invalidMedicalConditions));
        }
    }
}