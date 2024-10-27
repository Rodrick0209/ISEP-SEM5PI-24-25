// using System;
// using System.Collections.Generic;
// using System.Threading.Tasks;
// using DDDSample1.Domain.Shared;
// using DDDSample1.Domain.StaffMembers;
// using DDDSample1.Domain.StaffLoggers;
// using DDDSample1.Domain.Specializations;
// using Moq;
// using Xunit;

// namespace DDDSample1.Tests.UnitTests.Domain.StaffMembers
// {
//     public class StaffServiceTests
//     {
//         private readonly Mock<IUnitOfWork> _unitOfWorkMock;
//         private readonly Mock<IStaffRepository> _staffRepositoryMock;
//         private readonly Mock<IAvailabilitySlotsRepository> _availabilitySlotsRepositoryMock;
//         private readonly Mock<ISpecializationRepository> _specializationRepositoryMock;
//         private readonly Mock<IEmailSender> _emailSenderMock;
//         private readonly Mock<IStaffLoggerRepository> _staffLoggerRepositoryMock;

//         private readonly StaffService _staffService;

//         public StaffServiceTests()
//         {
//             _unitOfWorkMock = new Mock<IUnitOfWork>();
//             _staffRepositoryMock = new Mock<IStaffRepository>();
//             _availabilitySlotsRepositoryMock = new Mock<IAvailabilitySlotsRepository>();
//             _specializationRepositoryMock = new Mock<ISpecializationRepository>();
//             _emailSenderMock = new Mock<IEmailSender>();
//             _staffLoggerRepositoryMock = new Mock<IStaffLoggerRepository>();

//             _staffService = new StaffService(
//                 _unitOfWorkMock.Object,
//                 _staffRepositoryMock.Object,
//                 _availabilitySlotsRepositoryMock.Object,
//                 _specializationRepositoryMock.Object,
//                 _emailSenderMock.Object,
//                 _staffLoggerRepositoryMock.Object);
//         }

//         [Fact]
//         public async Task AddAsync_ValidStaff_AddsStaff()
//         {
//             // Arrange
//             var staffDto = new StaffDto
//             {
//                 Email = "test@example.com",
//                 PhoneNumber = "123456789",
//                 SpecializationId = "specializationId",
//                 Category = "Nurse"
//             };

//             _staffRepositoryMock.Setup(repo => repo.GetByEmailAsync(staffDto.Email)).ReturnsAsync((Staff)null);
//             _staffRepositoryMock.Setup(repo => repo.GetByPhoneNumberAsync(staffDto.PhoneNumber)).ReturnsAsync((Staff)null);
//             _specializationRepositoryMock.Setup(repo => repo.GetByIdAsync(It.IsAny<SpecializationId>())).ReturnsAsync(new Specialization());

//             // Act
//             var result = await _staffService.AddAsync(staffDto);

//             // Assert
//             _staffRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<Staff>()), Times.Once);
//             _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
//             Assert.NotNull(result);
//             Assert.Equal(staffDto.Email, result.Email);
//         }

//         [Fact]
//         public async Task UpdateAsync_ExistingStaff_UpdatesStaff()
//         {
//             // Arrange
//             var existingStaff = new Staff { Id = new StaffId("1"), Email = new Email("old@example.com"), FullName = new FullName("John Doe") };
//             var updateDto = new EditingStaffProfileDto { Id = existingStaff.Id.ToString(), Email = "new@example.com", FullName = "John Smith" };

//             _staffRepositoryMock.Setup(repo => repo.GetByIdAsync(existingStaff.Id)).ReturnsAsync(existingStaff);
//             _staffRepositoryMock.Setup(repo => repo.GetByEmailAsync(updateDto.Email)).ReturnsAsync((Staff)null);

//             // Act
//             var result = await _staffService.UpdateAsync(updateDto);

//             // Assert
//             Assert.Equal("new@example.com", result.Email);
//             _staffRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<Staff>()), Times.Never); // Não deve adicionar um novo
//             _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
//         }

//         [Fact]
//         public async Task DeleteAsync_ExistingStaff_DeletesStaff()
//         {
//             // Arrange
//             var staffId = new StaffId("1");
//             var existingStaff = new Staff { Id = staffId };

//             _staffRepositoryMock.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync(existingStaff);

//             // Act
//             var result = await _staffService.DeleteAsync(staffId);

//             // Assert
//             Assert.Equal(existingStaff, result);
//             _staffRepositoryMock.Verify(repo => repo.GetByIdAsync(staffId), Times.Once);
//             _staffLoggerRepositoryMock.Verify(repo => repo.AddAsync(It.IsAny<StaffLogger>()), Times.Once);
//             _unitOfWorkMock.Verify(uow => uow.CommitAsync(), Times.Once);
//         }

//         [Fact]
//         public async Task GetByIdAsync_ExistingStaff_ReturnsStaff()
//         {
//             // Arrange
//             var staffId = new StaffId("1");
//             var existingStaff = new Staff { Id = staffId };

//             _staffRepositoryMock.Setup(repo => repo.GetByIdAsync(staffId)).ReturnsAsync(existingStaff);

//             // Act
//             var result = await _staffService.GetByIdAsync(staffId);

//             // Assert
//             Assert.Equal(existingStaff, result);
//         }

//         [Fact]
//         public async Task AddAsync_EmailNotUnique_ThrowsException()
//         {
//             // Arrange
//             var staffDto = new StaffDto { Email = "test@example.com" };
//             _staffRepositoryMock.Setup(repo => repo.GetByEmailAsync(staffDto.Email)).ReturnsAsync(new Staff());

//             // Act & Assert
//             await Assert.ThrowsAsync<BusinessRuleValidationException>(() => _staffService.AddAsync(staffDto));
//         }
//     }
// }
