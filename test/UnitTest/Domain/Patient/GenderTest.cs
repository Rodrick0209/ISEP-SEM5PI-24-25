using System;
using DDDSample1.Domain.Patient;
using Xunit;

namespace UnitTest.Domain.Patient
{
    public class GenderTest
    {
        [Fact]
        public void CreateGender_WithValidGender_ShouldCreateGender()
        {
            // Arrange
            string validGender = "male";

            // Act
            Gender gender = new Gender(validGender);

            // Assert
            Assert.Equal(validGender, gender.gender);
        }

        [Fact]
        public void CreateGender_WithInvalidGender_ShouldThrowArgumentNullException()
        {
            // Arrange
            string invalidGender = "invalid";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Gender(invalidGender));
        }

        [Fact]
        public void CreateGender_WithEmptyGender_ShouldThrowArgumentNullException()
        {
            // Arrange
            string emptyGender = "";

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Gender(emptyGender));
        }

        [Fact]
        public void CreateGender_WithNullGender_ShouldThrowArgumentNullException()
        {
            // Arrange
            string? nullGender = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Gender(nullGender));
        }
    }
}