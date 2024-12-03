using DDDSample1.Controllers;
using DDDSample1.Domain.RoomTypes;
using DDDSample1.Domain.Shared;
using MastersData.Domain.RoomTypes;
using Microsoft.AspNetCore.Mvc;
using Moq;

namespace DDDSample1.Tests.IntegrationTests.Controllers
{
    public class RoomTypesControllerTest
    {
        private Mock<IRoomTypeRepository>? _roomTypeRepository;
        private Mock<IUnitOfWork>? _unitOfWork;
        private IRoomTypeService? _roomTypeService;
        private RoomTypesController? _roomTypesController;

        [Fact]
        public async Task AddAsync_ShouldSaveRoomType()
        {

            _roomTypeRepository = new Mock<IRoomTypeRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _roomTypeService = new RoomTypeService(_unitOfWork.Object, _roomTypeRepository.Object);
            _roomTypesController = new RoomTypesController(_roomTypeService);

            // Arrange
            var dto = new AddRoomTypeDto 
            { 
                InternalCode = "IC1-0001", 
                Designation = "Standard Room", 
                SuitableForSurgeries = true 
            };

            var roomType = new RoomType(dto.InternalCode, dto.Designation, null, dto.SuitableForSurgeries);
            _roomTypeRepository.Setup(repo => repo.AddAsync(It.IsAny<RoomType>())).ReturnsAsync(roomType);
            _unitOfWork.Setup(uow => uow.CommitAsync()).ReturnsAsync(1);

            // Act
            var result = await _roomTypesController.AddAsync(dto);

            // Assert
            var actionResult = Assert.IsType<CreatedAtActionResult>(result.Result);
            var returnValue = Assert.IsType<RoomTypeDto>(actionResult.Value);
            Assert.Equal("IC1-0001", returnValue.InternalCode);
            Assert.Equal("Standard Room", returnValue.Designation);
            Assert.True(returnValue.SuitableForSurgeries);
        }

        [Fact]
        public async Task GetById_ShouldReturnRoomType()
        {
            _roomTypeRepository = new Mock<IRoomTypeRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _roomTypeService = new RoomTypeService(_unitOfWork.Object, _roomTypeRepository.Object);
            _roomTypesController = new RoomTypesController(_roomTypeService);

            // Arrange
            var id = Guid.NewGuid();
            var roomTypeId = new RoomTypeId(id);
            var roomType = new RoomType("IC1-0001", "Standard Room Description", null, true);
            _roomTypeRepository.Setup(repo => repo.GetByIdAsync(It.IsAny<RoomTypeId>())).ReturnsAsync(roomType);

            // Act
            var result = await _roomTypesController.GetById(id);

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<RoomTypeDto>(actionResult.Value);
            Assert.Equal("IC1-0001", returnValue.InternalCode);
            Assert.Equal("Standard Room Description", returnValue.Designation);
            Assert.True(returnValue.SuitableForSurgeries);
        }

        [Fact]
        public async Task GetAll_ShouldReturnAllRoomTypes()
        {
            _roomTypeRepository = new Mock<IRoomTypeRepository>();
            _unitOfWork = new Mock<IUnitOfWork>();
            _roomTypeService = new RoomTypeService(_unitOfWork.Object, _roomTypeRepository.Object);
            _roomTypesController = new RoomTypesController(_roomTypeService);

            // Arrange
            var roomTypes = new List<RoomType>
                    {
                        new RoomType("IC1-0002", "Standard", "Standard Room Description", true),
                        new RoomType("IC1-0003", "Deluxe", "Deluxe Room Description", true)
                    };
            _roomTypeRepository.Setup(repo => repo.GetAllAsync()).ReturnsAsync(roomTypes);

            // Act
            var result = await _roomTypesController.GetAll();

            // Assert
            var actionResult = Assert.IsType<OkObjectResult>(result.Result);
            var returnValue = Assert.IsType<List<RoomTypeDto>>(actionResult.Value);
            Assert.Equal(2, returnValue.Count);
        }
    }
}