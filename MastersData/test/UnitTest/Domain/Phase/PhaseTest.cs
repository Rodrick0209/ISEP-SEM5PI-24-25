using System;
using System.Collections.Generic;
using Xunit;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Tests.Domain.OperationTypes
{
    public class PhaseTests
    {
        [Fact]
        public void CreatePhase_WithValidParameters_ShouldCreatePhase()
        {
            // Arrange
            var duration = 120; // Duração de 120 minutos
            var requiredStaff = new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            };

            // Act
            var phase = new Phase(duration, requiredStaff);

            // Assert
            Assert.NotNull(phase);
            Assert.Equal(duration, phase.duration);
            Assert.Equal(requiredStaff, phase.requiredStaff);
            Assert.NotEqual(Guid.Empty.ToString(), phase.Id.Value); // Verifica se o Guid foi gerado corretamente
        }

        [Fact]
        public void CreatePhase_WithInvalidDuration_ShouldThrowArgumentException()
        {
            // Arrange
            var invalidDuration = -10; // Duração inválida
            var requiredStaff = new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            };

            // Act & Assert
            Assert.Throws<ArgumentException>(() => new Phase(invalidDuration, requiredStaff));
        }

        [Fact]
        public void CreatePhase_WithNullRequiredStaff_ShouldThrowArgumentNullException()
        {
            // Arrange
            var duration = 90; // Duração válida
            List<RequiredStaff> nullRequiredStaff = null;

            // Act & Assert
            Assert.Throws<ArgumentNullException>(() => new Phase(duration, nullRequiredStaff));
        }


        [Fact]
        public void CreatePhase_WithValidId_ShouldCreatePhaseWithProvidedId()
        {
            // Arrange
            var phaseId = Guid.NewGuid();
            var duration = 100;
            var requiredStaff = new List<RequiredStaff> {
                new RequiredStaff(1, new SpecializationId(Guid.NewGuid()))
            };

            // Act
            var phase = new Phase(phaseId, duration, requiredStaff);

            // Assert
            Assert.NotNull(phase);
            Assert.Equal(phaseId.ToString(), phase.Id.Value);
            Assert.Equal(duration, phase.duration);
            Assert.Equal(requiredStaff, phase.requiredStaff);
        }
    }
}
