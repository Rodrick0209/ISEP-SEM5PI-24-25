using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;
using Xunit.Abstractions;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class OperationTypeControllerTestWithIsolation
    {
        private readonly ITestOutputHelper _output;
        private readonly OperationTypeService _operationTypeService;
        private readonly ISpecializationService _specializationService;
        private readonly OperationTypeController _operationTypeController;
        private readonly Mock<IUnitOfWork> _unitOfWork;
        private readonly Mock<IPhasesRepository> _phaseRepository;
        private readonly Mock<IOperationTypeRepository> _operationTypeRepository;
        private readonly Mock<ISpecializationRepository> _specializationRepository;

        public OperationTypeControllerTestWithIsolation(ITestOutputHelper output)
        {
            _output = output;
            _unitOfWork = new Mock<IUnitOfWork>();
            _phaseRepository = new Mock<IPhasesRepository>();
            _operationTypeRepository = new Mock<IOperationTypeRepository>();
            _specializationRepository = new Mock<ISpecializationRepository>();

            _operationTypeService = new OperationTypeService(_unitOfWork.Object, _operationTypeRepository.Object, _phaseRepository.Object);
            _specializationService = (ISpecializationService)new SpecializationService(_unitOfWork.Object, _specializationRepository.Object);

            _operationTypeController = new OperationTypeController(
                _operationTypeService,
                _specializationService
            );
        }

        [Fact]
        public async Task CreateAsync_WithValidDto_ReturnsCreatedResult()
        {
            var dto = new OperationTypeDto(
                Guid.NewGuid(),
                "test Operation Type",
                "active",
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111111"),
                    Duration = 20,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", "11111111-1111-1111-1111-111111111113"),
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111112"),
                    Duration = 90,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111113"),
                    Duration = 15,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", "11111111-1111-1111-1111-111111111113")
                    }
                },
                "11111111-1111-1111-1111-111111111113"
            );

            var operationType = new OperationType(dto.Name, dto.Status == "active",
                new Phase(dto.PreparationPhase.Duration, dto.PreparationPhase.RequiredStaff.Select(rs => new RequiredStaff(int.Parse(rs.num), new SpecializationId(Guid.Parse(rs.Specialization)))).ToList()),
                new Phase(dto.SurgeryPhase.Duration, dto.SurgeryPhase.RequiredStaff.Select(rs => new RequiredStaff(int.Parse(rs.num), new SpecializationId(Guid.Parse(rs.Specialization)))).ToList()),
                new Phase(dto.CleaningPhase.Duration, dto.CleaningPhase.RequiredStaff.Select(rs => new RequiredStaff(int.Parse(rs.num), new SpecializationId(Guid.Parse(rs.Specialization)))).ToList()),
                new SpecializationId(Guid.Parse(dto.Specialization)));

            _operationTypeRepository.Setup(repo => repo.AddAsync(It.IsAny<OperationType>())).ReturnsAsync(operationType);
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _operationTypeController.Create(dto);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var createdAtActionResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(createdAtActionResult.Value);

            Assert.Equal(dto.Name, returnValue.Name);
            Assert.Equal(dto.Status, returnValue.Status);
        }

        [Fact]
        public async Task CreateAsync_BadRequest_WhenDtoIsNull()
        {
            // Act
            var result = await _operationTypeController.Create(null);

            // Assert
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }

        [Fact]
        public async Task CreateAsync_BadRequest_WhenNameIsEmpty()
        {
            var dto = new OperationTypeDto(
                Guid.NewGuid(),
                "",
                "active",
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111111"),
                    Duration = 20,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", "11111111-1111-1111-1111-111111111113"),
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111112"),
                    Duration = 90,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111113"),
                    Duration = 15,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", "11111111-1111-1111-1111-111111111113")
                    }
                },
                "11111111-1111-1111-1111-111111111113"
            );

            // Act
            await Assert.ThrowsAsync<ArgumentException>(() => _operationTypeController.Create(dto));
        }


        [Fact]
        public async Task CreateAsync_BadRequest_WhenSpecializationIsInvalid()
        {
            var dto = new OperationTypeDto(
                Guid.NewGuid(),
                "test Operation Type",
                "active",
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111111"),
                    Duration = 20,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("10", "11111111-1111-1111-1111-111111111113"),
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111112"),
                    Duration = 90,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("20", "11111111-1111-1111-1111-111111111113")
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.Parse("11111111-1111-1111-1111-111111111113"),
                    Duration = 15,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                        new RequiredStaffDTO("2", "11111111-1111-1111-1111-111111111113")
                    }
                },
                "invalid_specialization"
            );

            // Act
            await Assert.ThrowsAsync<FormatException>(() => _operationTypeController.Create(dto));

        }

        [Fact]
        public async Task GetByIdAsync_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            // Arrange
            var id = Guid.NewGuid().ToString();
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _operationTypeController.GetById(id);

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task GetByIdAsync_ReturnsOk_WhenOperationTypeExists()
        {
            // Arrange
            var id = Guid.NewGuid().ToString();
            var specializationId = new SpecializationId(Guid.NewGuid().ToString());

            var requiredStaff1 = new RequiredStaff(10, specializationId);
            var requiredStaff2 = new RequiredStaff(20, specializationId);
            var requiredStaffList1 = new List<RequiredStaff> { requiredStaff1, requiredStaff2 };
            var requiredStaffList2 = new List<RequiredStaff> { new RequiredStaff(20, specializationId) };
            var requiredStaffList3 = new List<RequiredStaff> { new RequiredStaff(2, specializationId) };

            var phase1 = new Phase(20, requiredStaffList1);
            var phase2 = new Phase(90, requiredStaffList2);
            var phase3 = new Phase(15, requiredStaffList3);

            var operationType = new OperationType("test Operation Type", true, phase1, phase2, phase3, specializationId);
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync(operationType);

            // Act
            var result = await _operationTypeController.GetById(id);

            // Assert
            var actionResult = Assert.IsType<ActionResult<OperationTypeDto>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<OperationTypeDto>(okResult.Value);

            Assert.Equal("test Operation Type", returnValue.Name);
            Assert.Equal(20, returnValue.PreparationPhase.Duration);
            Assert.Equal(90, returnValue.SurgeryPhase.Duration);
            Assert.Equal(15, returnValue.CleaningPhase.Duration);
        }


        [Fact]
        public async Task CreateAsync_BadRequest_WhenDurationIsNegative()
        {
            // Arrange
            var dto = new OperationTypeDto(
                Guid.NewGuid(),
                "test Operation Type",
                "active",
                new PhaseDTO
                {
                    Id = Guid.NewGuid(),
                    Duration = -10, // Invalid negative duration
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                new RequiredStaffDTO("10", Guid.NewGuid().ToString())
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.NewGuid(),
                    Duration = 90,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                new RequiredStaffDTO("20", Guid.NewGuid().ToString())
                    }
                },
                new PhaseDTO
                {
                    Id = Guid.NewGuid(),
                    Duration = 15,
                    RequiredStaff = new List<RequiredStaffDTO>
                    {
                new RequiredStaffDTO("5", Guid.NewGuid().ToString())
                    }
                },
                Guid.NewGuid().ToString()
            );

            await Assert.ThrowsAsync<ArgumentException>(() => _operationTypeController.Create(dto));

           
        }

        [Fact]
        public async Task GetAll_ReturnsEmptyList_WhenNoOperationTypesExist()
        {
            // Arrange
            _operationTypeRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(new List<OperationType>());

            // Act
            var result = await _operationTypeController.GetAll();

            // Assert
            var actionResult = Assert.IsType<ActionResult<IEnumerable<OperationTypeDto>>>(result);
            var okResult = Assert.IsType<OkObjectResult>(actionResult.Result);
            var returnValue = Assert.IsType<List<OperationTypeDto>>(okResult.Value);

            Assert.Empty(returnValue);
        }

        [Fact]
        public async Task UpdateAsync_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            // Arrange
            var id = Guid.NewGuid();
            var dto = new OperationTypeDto(id, "Updated Name", "inactive", null, null, null, null);
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _operationTypeController.Update(id, dto);

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }

        [Fact]
        public async Task DeleteAsync_ReturnsNotFound_WhenOperationTypeDoesNotExist()
        {
            // Arrange
            var id = Guid.NewGuid();
            _operationTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<OperationTypeId>())).ReturnsAsync((OperationType)null);

            // Act
            var result = await _operationTypeController.Inactivate(id.ToString());

            // Assert
            Assert.IsType<NotFoundResult>(result.Result);
        }


    }
}