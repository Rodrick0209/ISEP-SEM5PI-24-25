using System;
using Xunit;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Tests.Domain.Specializations
{
    public class SpecializationIdTest
    {
        [Fact]
        public void TestConstructorWithGuid()
        {
            // Arrange
            var guid = Guid.NewGuid();

            // Act
            var specializationId = new SpecializationId(guid);

            // Assert
            Assert.Equal(guid, specializationId.AsGuid());
        }

        [Fact]
        public void TestConstructorWithString()
        {
            // Arrange
            var guidString = Guid.NewGuid().ToString();

            // Act
            var specializationId = new SpecializationId(guidString);

            // Assert
            Assert.Equal(guidString, specializationId.AsString());
        }

        [Fact]
        public void TestAsString()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var specializationId = new SpecializationId(guid);

            // Act
            var result = specializationId.AsString();

            // Assert
            Assert.Equal(guid.ToString(), result);
        }

        [Fact]
        public void TestAsGuid()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var specializationId = new SpecializationId(guid);

            // Act
            var result = specializationId.AsGuid();

            // Assert
            Assert.Equal(guid, result);
        }

        [Fact]
        public void TestCreateFromString()
        {
            // Arrange
            var guidString = Guid.NewGuid().ToString();
            var specializationId = new SpecializationId(guidString);

            // Act
            var result = specializationId.AsGuid();

            // Assert
            Assert.Equal(new Guid(guidString), result);
        }

        [Fact]
        public void TestInvalidGuidString()
        {
            // Arrange
            var invalidGuidString = "invalid-guid";

            // Act & Assert
            Assert.Throws<FormatException>(() => new SpecializationId(invalidGuidString));
        }
    }
}