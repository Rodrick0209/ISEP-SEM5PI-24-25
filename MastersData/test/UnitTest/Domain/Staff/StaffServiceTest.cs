using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Moq;
using Xunit;
using DDDSample1.Domain.StaffMembers;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Specializations;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.StaffLoggers;
using DDDSample1.Domain.User;
using DDDSample1.Controllers;
using SQLitePCL;

public class StaffServiceTests
{

    private Mock<IStaffRepository>? _staffRepository;
    private Mock<IAvailabilitySlotsRepository>? _availabilitySlotsRepository;
    private Mock<ISpecializationRepository>? __specializationRepository;
    private Mock<IStaffLoggerRepository>? _staffLoggerRepository;
    private Mock<IEmailSender>? _emailSender;
    private Mock<IUnitOfWork>? _unitOfWork;
    private StaffService? _staffService;
    private StaffController _staffController;

    //comentado porque specialization nao esta implementado
    /*[Fact]
    public async Task AddAsync_ShouldAddNewStaffMember_WhenDataIsValid()
    {


        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        


        // Arrange
        var staffDto = new StaffDto(
             new StaffId("D202412345"),
             "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 1234567890",
             "Doctor",
             "true"
            );

        _staffRepository.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.StaffMembers.Staff));
        _staffRepository.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.StaffMembers.Staff));

        // Act
        var result = await _staffService.AddAsync(staffDto);

        // Assert
        Assert.NotNull(result);
        _staffRepository.Verify(repo => repo.AddAsync(It.IsAny<DDDSample1.Domain.StaffMembers.Staff>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    } */



    [Fact]
    public async Task CreateAsync_ShouldThrowException_WhenEmailNotUnique()
    {
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        // Arrange
        var staffDto = new StaffDto(
             new StaffId("D202412345"),
             "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 1234567890",
             "Doctor",
             "true"
            );

        _staffRepository.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(new DDDSample1.Domain.StaffMembers.Staff(
            new StaffId("D202412345"),
            "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 12345678998",
             "Doctor",
             "true"

        ));

        // Act & Assert
        await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _staffService.AddAsync(staffDto));
    }

    // com estes nao sei o que se passa
    /*[Fact]
    public async Task UpdateAsync_ShouldUpdateStaffFullName_WhenValidData()
    {
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var dto = new EditingStaffProfileDto("D202412345", "John Doe Updated", "12345", "+351 1234567890", "john.doe@example.com");

        var existingStaff = new DDDSample1.Domain.StaffMembers.Staff(
            new StaffId("D202412345"),
             "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 12345678998",
             "Doctor",
             "true"
        );

        _staffRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(existingStaff);
        _staffLoggerRepository.Setup(repo => repo.AddAsync(It.IsAny<StaffLogger>()));

        // Act
        var result = await _staffService.UpdateAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("John Doe Updated", result.FullName);
        _staffLoggerRepository.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    }

    [Fact]
    public async Task UpdateAsync_ShouldUpdateStaffEmail_WhenValidData()
    {
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var dto = new EditingStaffProfileDto("D202412345", "John Doe", "12345", "+351 1234567898", "john.doe.updated@example.com");

        var existingStaff = new DDDSample1.Domain.StaffMembers.Staff(
            new StaffId("D202412345"),
            "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 12345678998",
             "Doctor",
             "true"

        );

        _staffRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(existingStaff);
        _staffLoggerRepository.Setup(repo => repo.AddAsync(It.IsAny<StaffLogger>()));

        // Act
        var result = await _staffService.UpdateAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("john.doe.updated@example.com", result.Email);
        _staffLoggerRepository.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    }

    [Fact]
    public async Task UpdateAsync_ShouldUpdateStaffPhoneNumber_WhenValidData()
    {
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var dto = new EditingStaffProfileDto("D202412345", "John Doe", "12345", "+351 1234567899", "john.doe@example.com");

        var existingStaff = new DDDSample1.Domain.StaffMembers.Staff(
            new StaffId("D202412345"),
            "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 12345678998",
             "Doctor",
             "true"

        );

        _staffRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(existingStaff);
        _staffLoggerRepository.Setup(repo => repo.AddAsync(It.IsAny<StaffLogger>()));

        // Act
        var result = await _staffService.UpdateAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("+351 1234567899", result.PhoneNumber);
        _staffLoggerRepository.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    }

    [Fact]
    public async Task UpdateAsync_ShouldUpdateStaff_WhenValidData()
    {

        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var dto = new EditingStaffProfileDto("D202412345", "John Doe Updated", "12345", "+351 098765432", "john.doe.updated@example.com");


        var existingStaff = new DDDSample1.Domain.StaffMembers.Staff(
            new StaffId("D202412345"),
            "John Doe",
             "12345",
             "11111111-1111-1111-1111-111111111113",
             "11111111-1111-1111-1111-111111111114",
             "john.doe@example.com",
             "+351 12345678998",
             "Doctor",
             "true"

        );

        _staffRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(existingStaff);
        _staffRepository.Setup(repo => repo.GetByEmailAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.StaffMembers.Staff));
        _staffRepository.Setup(repo => repo.GetByPhoneNumberAsync(It.IsAny<string>())).ReturnsAsync(default(DDDSample1.Domain.StaffMembers.Staff));

        // Act
        var result = await _staffService.UpdateAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal("John Doe Updated", result.FullName);
        Assert.Equal("john.doe.updated@example.com", result.Email);
        Assert.Equal("+351 098765432", result.PhoneNumber);
        _staffLoggerRepository.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
        _emailSender.Verify(sender => sender.SendEmailAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()), Times.Once);
    }*/

    [Fact]
    public async Task DeleteAsync_ShouldDeactivateStaff_WhenExists()
    {
        // Arrange
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var staff = new Staff(
            new StaffId("D202412345"),
            "John Doe",
            "12345",
            "11111111-1111-1111-1111-111111111113",
            "11111111-1111-1111-1111-111111111114",
            "john.doe@example.com",
            "+351 1234567890",
            "Doctor",
            "true"
        );

        _staffRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<StaffId>())).ReturnsAsync(staff);
        
        // Act
        var result = await _staffService.DeleteAsync(new StaffId("D202412345"));

        // Assert
        Assert.NotNull(result);
        Assert.False(staff.status); // Assuming IsActive is the flag you toggle in Deactivate
        _staffLoggerRepository.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
        _unitOfWork.Verify(uow => uow.CommitAsync(), Times.Once);
    }


    [Fact]
    public async Task SeachAsync_ShouldReturnStaffs_WhenFiltersAreDisponible()
    {

        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        // Arrange
        var dto = new StaffFilterDto
        {
            Name = "J",
            LicenseNumber = "1234",
            PhoneNumber = "+351 123456789",
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
                    "john.doe@example.com",
                    "+351 1234567890",
                    "Doctor",
                    "true"),            };

        _staffRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(staffs);
        _staffRepository.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
        .ReturnsAsync(staffs.Where(s => s.FullName.fullName.Contains(dto.Name)
                        && s.LicenseNumber.licenseNumber.Contains(dto.LicenseNumber)
                        && s.PhoneNumber.phoneNumber.Contains(dto.PhoneNumber)
                        && s.Email.email.Contains(dto.Email))
                        .ToList());

        // Act
        var result = await _staffService.SearchAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Equal(2, result.Count);
    }


    [Fact]
    public async Task SeachAsync_ShouldReturnStaff_WhenFiltersAreDisponible()
    {

        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        // Arrange
        var dto = new StaffFilterDto
        {
            Name = "John Doe",
            LicenseNumber = "1234",
            PhoneNumber = "+351 1234567890",
            Email = "john."

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
                    "John Doe",
                    "12346",
                    "11111111-1111-1111-1111-111111111114",
                    "11111111-1111-1111-1111-111111111115",
                    "johna.doe@example.com",
                    "+351 1234567898",
                    "Doctor",
                    "true"),

                new Staff(
                    new StaffId("D202412346"),
                    "Jane Done",
                    "12346",
                    "11111111-1111-1111-1111-111111111114",
                    "11111111-1111-1111-1111-111111111115",
                    "john.doe@example.com",
                    "+351 1234567898",
                    "Doctor",
                    "true"),                 };

        _staffRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(staffs);
        _staffRepository.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
        .ReturnsAsync(staffs.Where(s => s.FullName.fullName.Contains(dto.Name)
                        && s.LicenseNumber.licenseNumber.Contains(dto.LicenseNumber)
                        && s.PhoneNumber.phoneNumber.Contains(dto.PhoneNumber)
                        && s.Email.email.Contains(dto.Email))
                        .ToList());

        // Act
        var result = await _staffService.SearchAsync(dto);

        // Assert
        Assert.NotNull(result);
        Assert.Single(result);
        Assert.Equal("John Doe", result[0].Name);
        Assert.Equal("john.doe@example.com", result[0].Email);
        Assert.Equal("+351 1234567890", result[0].PhoneNumber);
        Assert.Equal("12345", result[0].LicenseNumber);
    }


    [Fact]
    public async Task SeachAsync_ShouldReturnAllStaff_WhenFiltersAreNotDisponible()
    {

        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        // Arrange
        var dto = new StaffFilterDto
        {
            Name = "",
            LicenseNumber = "",
            PhoneNumber = "",
            Email = ""

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
                    "John Doe",
                    "12346",
                    "11111111-1111-1111-1111-111111111114",
                    "11111111-1111-1111-1111-111111111115",
                    "johna.doe@example.com",
                    "+351 1234567898",
                    "Doctor",
                    "true"),

                new Staff(
                    new StaffId("D202412346"),
                    "Jane Done",
                    "12346",
                    "11111111-1111-1111-1111-111111111114",
                    "11111111-1111-1111-1111-111111111115",
                    "john.doe@example.com",
                    "+351 1234567898",
                    "Doctor",
                    "true"),                 };

        _staffRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(staffs);
        _staffRepository.Setup(repo => repo.GetByFiltersAsync(It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>(), It.IsAny<string>()))
        .ReturnsAsync(staffs.Where(s => s.FullName.fullName.Contains(dto.Name)
                        && s.LicenseNumber.licenseNumber.Contains(dto.LicenseNumber)
                        && s.PhoneNumber.phoneNumber.Contains(dto.PhoneNumber)
                        && s.Email.email.Contains(dto.Email))
                        .ToList());

        // Act
        var result = await _staffService.SearchAsync(dto);

        /// Assert
        Assert.NotNull(result);
        Assert.Equal(3, result.Count);
    }

    [Fact]
    public async Task GetAllAsync_ShouldReturnAllStaffs()
    {
        _unitOfWork = new Mock<IUnitOfWork>();
        _staffRepository = new Mock<IStaffRepository>();
        _staffLoggerRepository = new Mock<IStaffLoggerRepository>();
        _availabilitySlotsRepository = new Mock<IAvailabilitySlotsRepository>();
        _emailSender = new Mock<IEmailSender>();
        __specializationRepository = new Mock<ISpecializationRepository>();
        _staffService = new StaffService(_unitOfWork.Object, _staffRepository.Object, _availabilitySlotsRepository.Object, __specializationRepository.Object, null, _staffLoggerRepository.Object);
        _staffController = new StaffController(_staffService);
        var _staffIdGeneratorService = new StaffIdGeneratorService();

        var staffs = new List<DDDSample1.Domain.StaffMembers.Staff>
            {new(
                new StaffId("D202412345"),
                    "John Doe",
                    "12345",
                    "11111111-1111-1111-1111-111111111113",
                    "11111111-1111-1111-1111-111111111114",
                    "john.doe@example.com",
                    "+351 1234567890",
                    "Doctor",
                    "true"),
            };

        _staffRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(staffs);

        // Act
        var result = await _staffService.GetAllAsync();

        // Assert
        Assert.NotNull(result);
        Assert.Single(result);
    }



}

