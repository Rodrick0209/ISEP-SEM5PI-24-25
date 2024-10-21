using System;
using Xunit;

namespace DDDSample1.Domain.OperationTypes.Tests
{
    public class PhasesIdTests
    {
        [Fact]
        public void Constructor_WithGuid_ShouldCreatePhasesId()
        {
            // Arrange
            var guid = Guid.NewGuid();

            // Act
            var phasesId = new PhasesId(guid);

            // Assert
            Assert.Equal(guid, phasesId.AsGuid());
        }

        [Fact]
        public void Constructor_WithString_ShouldCreatePhasesId()
        {
            // Arrange
            var guidString = Guid.NewGuid().ToString();

            // Act
            var phasesId = new PhasesId(guidString);

            // Assert
            Assert.Equal(guidString, phasesId.AsString());
        }

        [Fact]
        public void AsString_ShouldReturnCorrectString()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var phasesId = new PhasesId(guid);

            // Act
            var result = phasesId.AsString();

            // Assert
            Assert.Equal(guid.ToString(), result);
        }

        [Fact]
        public void AsGuid_ShouldReturnCorrectGuid()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var phasesId = new PhasesId(guid);

            // Act
            var result = phasesId.AsGuid();

            // Assert
            Assert.Equal(guid, result);
        }

    }
}