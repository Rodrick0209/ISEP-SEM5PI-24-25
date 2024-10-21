using System;
using Xunit;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Tests.Domain.OperationTypes
{
    public class RequiredStaffTests
    {
        [Fact]
        public void CreateRequiredStaff_WithValidParameters_ShouldCreateRequiredStaff()
        {
            // Arrange
            var num = 2;
            var specializationId = new SpecializationId(Guid.NewGuid());

            // Act
            var requiredStaff = new RequiredStaff(num, specializationId);

            // Assert
            Assert.NotNull(requiredStaff);
            Assert.Equal(num, requiredStaff.num);
            Assert.Equal(specializationId, requiredStaff.specialization);
        }

        [Fact]
        public void CreateRequiredStaff_WithNullSpecialization_ShouldThrowArgumentNullException()
        {
            // Arrange
            var num = 1;
            SpecializationId specializationId = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new RequiredStaff(num, specializationId));
        }

        [Fact]
        public void CreateRequiredStaff_WithNegativeNum_ShouldThrowArgumentException()
        {
            // Arrange
            var invalidNum = -1;
            var specializationId = new SpecializationId(Guid.NewGuid());

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new RequiredStaff(invalidNum, specializationId));
        }

        [Fact]
        public void CreateRequiredStaff_WithZeroNum_ShouldThrowArgumentException()
        {
            // Arrange
            var invalidNum = 0;
            var specializationId = new SpecializationId(Guid.NewGuid());

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new RequiredStaff(invalidNum, specializationId));
        }
    }
}
