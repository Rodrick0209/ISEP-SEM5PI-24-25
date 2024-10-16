using System;
using Xunit;
using DDDSample1.Domain.Patient;

namespace DDDSample1.UnitTests.Domain.Patient
{
    public class DateOfBirthTest
    {
        [Fact]
        public void Should_Create_DateOfBirth_With_Valid_Date()
        {
            // Arrange
            DateTime validDate = new DateTime(1990, 1, 1);

            // Act
            DateOfBirth dateOfBirth = new DateOfBirth(validDate);

            // Assert
            Assert.Equal(validDate, dateOfBirth.dateOfBirth);
        }

        [Fact]
        public void Should_Throw_Exception_For_Future_Date()
        {
            // Arrange
            DateTime futureDate = DateTime.Now.AddDays(1);

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new DateOfBirth(futureDate));
        }
    }
}

