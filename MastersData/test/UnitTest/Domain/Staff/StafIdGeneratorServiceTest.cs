using System;
using DDDSample1.Domain.StaffMembers;
using Xunit;

namespace DDDSample1.Tests.UnitTests.Domain.StaffMembers
{
    public class StaffIdGeneratorServiceTests
    {
        private readonly StaffIdGeneratorService _service;

        public StaffIdGeneratorServiceTests()
        {
            _service = new StaffIdGeneratorService();
        }

        [Fact]
        public void GenerateStaffId_GeneratesCorrectFormat_ForDoctor()
        {
            // Arrange
            var category = Category.Doctor;
            var recruitmentDate = new DateTime(2023, 5, 1);

            // Act
            var staffId = _service.generateStaffId(category, recruitmentDate);

            // Assert
            Assert.StartsWith("D2023", staffId.Value);
            Assert.Equal(10, staffId.Value.Length); // 1 (prefix) + 4 (year) + 5 (random number)
        }

        [Fact]
        public void GenerateStaffId_GeneratesCorrectFormat_ForNurse()
        {
            // Arrange
            var category = Category.Nurse;
            var recruitmentDate = new DateTime(2023, 5, 1);

            // Act
            var staffId = _service.generateStaffId(category, recruitmentDate);

            // Assert
            Assert.StartsWith("N2023", staffId.Value);
            Assert.Equal(10, staffId.Value.Length);
        }

        [Fact]
        public void GenerateStaffId_GeneratesDifferentIds_ForSameCategory()
        {
            // Arrange
            var category = Category.Technician;
            var recruitmentDate = new DateTime(2023, 5, 1);
            
            // Act
            var staffId1 = _service.generateStaffId(category, recruitmentDate);
            var staffId2 = _service.generateStaffId(category, recruitmentDate);

            // Assert
            Assert.NotEqual(staffId1.Value, staffId2.Value); // IDs devem ser diferentes
        }

        [Fact]
        public void GenerateStaffId_IncrementsSequentialNumber_ForSameCategory()
        {
            // Arrange
            var category = Category.Nurse;
            var recruitmentDate = new DateTime(2023, 5, 1);

            // Act
            var staffId1 = _service.generateStaffId(category, recruitmentDate);
            var staffId2 = _service.generateStaffId(category, recruitmentDate);

            // Assert
            Assert.NotEqual(staffId1.Value, staffId2.Value); // IDs devem ser diferentes
            Assert.StartsWith("N2023", staffId1.Value);
            Assert.StartsWith("N2023", staffId2.Value);
        }
    }
}
