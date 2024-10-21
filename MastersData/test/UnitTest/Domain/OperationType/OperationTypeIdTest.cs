using System;
using Xunit;
using DDDSample1.Domain.OperationTypes;

namespace DDDSample1.Tests.Domain.OperationTypes
{
    public class OperationTypeIdTest
    {
        [Fact]
        public void TestConstructorWithGuid()
        {
            // Arrange
            var guid = Guid.NewGuid();

            // Act
            var operationTypeId = new OperationTypeId(guid);

            // Assert
            Assert.Equal(guid, operationTypeId.AsGuid());
        }

        [Fact]
        public void TestConstructorWithString()
        {
            // Arrange
            var guidString = Guid.NewGuid().ToString();

            // Act
            var operationTypeId = new OperationTypeId(guidString);

            // Assert
            Assert.Equal(guidString, operationTypeId.AsString());
        }

        [Fact]
        public void TestAsString()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var operationTypeId = new OperationTypeId(guid);

            // Act
            var result = operationTypeId.AsString();

            // Assert
            Assert.Equal(guid.ToString(), result);
        }

        [Fact]
        public void TestAsGuid()
        {
            // Arrange
            var guid = Guid.NewGuid();
            var operationTypeId = new OperationTypeId(guid);

            // Act
            var result = operationTypeId.AsGuid();

            // Assert
            Assert.Equal(guid, result);
        }

        [Fact]
        public void TestCreateFromString()
        {
            // Arrange
            var guidString = Guid.NewGuid().ToString();
            var operationTypeId = new OperationTypeId(guidString);

            // Act
            var result = operationTypeId.AsGuid();

            // Assert
            Assert.Equal(new Guid(guidString), result);
        }

        [Fact]
        public void TestInvalidGuidString()
        {
            // Arrange
            var invalidGuidString = "invalid-guid";

            // Act & Assert
            Assert.Throws<FormatException>(() => new OperationTypeId(invalidGuidString));
        }
    }
}