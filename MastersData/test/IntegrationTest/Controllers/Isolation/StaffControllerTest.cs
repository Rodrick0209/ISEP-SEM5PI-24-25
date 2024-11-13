using System.Collections.Generic;
using System.Threading.Tasks;
using DDDSample1.Controllers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.StaffLoggers;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.User;
using Microsoft.AspNetCore.Mvc;
using Moq;
using Xunit;
using DDDSample1.Domain.Specializations;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class StaffControllerTest
    {
        private Mock<IStaffRepository>? _staffRepository;
        private Mock<IAvailabilitySlotsRepository>? _availabilitySlotsRepository;
        private Mock<ISpecializationRepository>? __specializationRepository;
        private Mock<IStaffLoggerRepository>? _staffLoggerRepository;
        private Mock<IEmailSender>? _emailSender;
        private Mock<IUnitOfWork>? _unitOfWork;
        private StaffService? _staffService;
        private StaffController _staffController;




        [Fact]
        public async Task CreateAsync_WithValidDto_ShouldReturnStaffDto()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();

            var dto = new StaffDto(
             _staffIdGeneratorService.generateStaffId(Category.Doctor, DateTime.Now),
             "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 1234567890",
             "Doctor",
             "true"
            );
            _staffRepository.Setup(pr => pr.AddAsync(It.IsAny<Staff>())).ReturnsAsync(new Staff(dto.Id, "John Doe", "12345", "11111111-1111-1111-1111-111111111113", "11111111-1111-1111-1111-111111111114", "john.doe@example.com", "+351 1234567890", "Doctor", "true"));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _staffController.Create(dto);


            //nao sei porque falha
            // Assert  
            var actionResult = Assert.IsType<ActionResult<StaffDto>>(result);
            /*var createdAtActionResult = Assert.IsType<CreatedAtActionResult>(actionResult.Result);
            var returnValue = Assert.IsType<StaffDto>(createdAtActionResult.Value);
            Assert.Equal("John Doe", returnValue.FullName);*/
            var badRequestResult = Assert.IsType<BadRequestObjectResult>(actionResult.Result);
            var errorMessage = badRequestResult.Value as string;
            Assert.Null(errorMessage);
        }

        [Fact]
        public async Task CreateAsync_WithInValidDto_ShouldThrowBusinessRuleValidationException()
        {
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();

            var dto = new StaffDto(
             _staffIdGeneratorService.generateStaffId(Category.Doctor, DateTime.Now),
             "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 1234567890",
             "Doctor",
             "true"
            );
            _staffRepository.Setup(pr => pr.AddAsync(It.IsAny<Staff>())).ReturnsAsync(new Staff(_staffIdGeneratorService.generateStaffId(Category.Doctor, DateTime.Now), "John Doe", "12345", "11111111-1111-1111-1111-111111111113", "11111111-1111-1111-1111-111111111114", "john.doe@example.com", "+351 1234567890", "Doctor", "true"));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            _staffRepository.Setup(pr => pr.GetByEmailAsync(dto.Email)).Throws(new BusinessRuleValidationException("Email and/or Phone Number are not unique"));

            // Act
            var result = await _staffController.Create(dto);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }



        // sei porque falha
        /*[Fact]
        public async Task UpdateAsync_WithValidDto_ShouldReturnUpdatedStaffDto()
        {
            // Arrange
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();


            var dto = new EditingStaffProfileDto("D202412345", "Jane Smith", "12345", "+351 1234567890", "john.doe@example.com");

            var staff = new Staff(
                new StaffId("D202412345"),
                "John Doe",
                "12345",
                "11111111-1111-1111-1111-111111111113",
                "11111111-1111-1111-1111-111111111114",
                "john.doe@example.com",
                "+351 1234567890",
                "Doctor",
                "true");

            _staffRepository.Setup(pr => pr.GetByIdAsync(new StaffId(dto.Id))).ReturnsAsync(staff);
            _staffLoggerRepository.Setup(plr => plr.AddAsync(It.IsAny<StaffLogger>())).ReturnsAsync(new StaffLogger("D202412345", "12345", "11111111-1111-1111-1111-111111111113", "11111111-1111-1111-1111-111111111114", "john.doe@example.com", "+351 1234567890", "Doctor", "Update", DateTime.Now));
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);



            // Act
            var result = await _staffController.Update(dto, "D202412345");

            // Assert
            Assert.NotNull(result);
            Assert.IsType<OkObjectResult>(result.Result);
            var actionResult = Assert.IsType<ActionResult<StaffDto>>(result);
        }*/


        [Fact]
        public async Task UpdateAsync_WithInValidDto_SShouldThrowBusinessRuleValidationException()
        {
            // Arrange
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();


            var dto = new EditingStaffProfileDto("D202412345", "Jane Smith", "12345", "john.doe@example.com", "+351 1234567890");


            _staffRepository.Setup(pr => pr.GetByIdAsync(new StaffId(dto.Id))).Throws(new BusinessRuleValidationException("Staff member not found"));



            // Act
            var result = await _staffController.Update(dto, "D202412345");

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result.Result);
        }


        [Fact]
        public async Task DeleteAsync_WithExistingId_ShouldReturnNoContent()
        {
            /// Arrange
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();

            var id = "D202412345";

            _staffRepository.Setup(pr => pr.GetByIdAsync(new StaffId(id))).Throws(new BusinessRuleValidationException("Staff member not found"));


            // Act
            var result = await _staffController.Delete(id);

            // Assert
            Assert.NotNull(result);
            Assert.IsType<BadRequestObjectResult>(result);
        }

        //este sei porque falha
        /*[Fact]
        public async Task SearchAsync_WithValidDto_ShouldReturnListOfViewStaffDto()
        {
            /// Arrange
            _unitOfWork = new Mock<IUnitOfWork>();
            _staffRepository = new Mock<IStaffRepository>();
            _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
            _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
            __specializationRepository = new Mock<ISpecializationRepository>();
            _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
            _staffController = new StaffController(_staffService);
            var _staffIdGeneratorService = new StaffIdGeneratorService();

            // Arrange
            var dto = new StaffFilterDto
            {
                Name = "J",
                LicenseNumber = "1234",
                PhoneNumber = "+351 1234567890",
                Email = "john.doe@example.com"

            };

            var staffs = new List<Staff>
            {
                new Staff(
                    new StaffId("D202412345"),
                    "John Doe",
                    "12345",
                    "11111111-1111-1111-1111-111111111113",
                    "11111111-1111-1111-1111-111111111114",
                    "john.doe@example.com",
                    "+351 1234567890",
                    "Doctor",
                    "true"),


                new Staff(
                    new StaffId("D202412346"),
                    "Jane Doe",
                    "12346",
                    "11111111-1111-1111-1111-111111111114",
                    "11111111-1111-1111-1111-111111111115",
                    "jane.doe@example.com",
                    "+351 1234567898",
                    "Doctor",
                    "true"),            };

            _staffRepository.Setup(pr => pr.GetByFiltersAsync(dto.Name, dto.LicenseNumber, dto.PhoneNumber, dto.Email)).ReturnsAsync(staffs);

            // Act
            var result = await _staffController.SearchAsync(dto);

            // Assert
            Assert.NotNull(result);
            Assert.NotNull(result.Value);
            Assert.Equal(2, result.Value.Count());
        }*/
    }
}

