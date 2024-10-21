using System;
using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Tests.Domain.OperationTypes
{
    public class OperationTypeTests
    {
        [Fact]
        public void CreateOperationType_WithValidParameters_ShouldCreateOperationType()
        {
            // Arrange
            var name = "Cardiac Surgery";
            var status = true;
            var specializationId = new SpecializationId(Guid.NewGuid());

            var preparationPhase = new Phase(30, new List<RequiredStaff> {
                new RequiredStaff(2, new SpecializationId(Guid.NewGuid()))
            });
            var surgeryPhase = new Phase(120, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });
            var cleaningPhase = new Phase(45, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });

            // Act
            var operationType = new OperationType(name, status, preparationPhase, surgeryPhase, cleaningPhase, specializationId);

            // Assert
            Assert.NotNull(operationType);
            Assert.Equal(name, operationType.name);
            Assert.Equal(status, operationType.status);
            Assert.Equal(preparationPhase, operationType.preparationPhase);
            Assert.Equal(surgeryPhase, operationType.surgeryPhase);
            Assert.Equal(cleaningPhase, operationType.cleaningPhase);
            Assert.Equal(specializationId, operationType.specialization);
        }

        [Fact]
        public void Deactivate_ShouldSetStatusToFalse()
        {
            // Arrange
            var name = "Cardiac Surgery";
            var status = true;
            var specializationId = new SpecializationId(Guid.NewGuid());

            var preparationPhase = new Phase(30, new List<RequiredStaff> {
                new RequiredStaff(2, new SpecializationId(Guid.NewGuid()))
            });
            var surgeryPhase = new Phase(120, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });
            var cleaningPhase = new Phase(45, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });

            var operationType = new OperationType(name, status, preparationPhase, surgeryPhase, cleaningPhase, specializationId);

            // Act
            operationType.Deactivate();

            // Assert
            Assert.False(operationType.status);
        }

        [Fact]
        public void CreateOperationType_WithNullSpecialization_ShouldThrowArgumentNullException()
        {
            // Arrange
            var name = "Cardiac Surgery";
            var status = true;
            SpecializationId specializationId = null;

            var preparationPhase = new Phase(30, new List<RequiredStaff> {
                new RequiredStaff(2, new SpecializationId(Guid.NewGuid()))
            });
            var surgeryPhase = new Phase(120, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });
            var cleaningPhase = new Phase(45, new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            });

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new OperationType(name, status, preparationPhase, surgeryPhase, cleaningPhase, specializationId));
        }
    }
}
