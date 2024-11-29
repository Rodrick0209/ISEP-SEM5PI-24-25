


        using System;
    using System.Collections.Generic;
using DDDSample1.Domain.Appointments;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.Utils;
using Moq;
using Xunit;
    
namespace UnitTest.Domain.Room
{


    public class OperationRoomTests
    {
        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenStartMinuteIsNegative()
        {
            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,null);
            var date = new DateOnly(2023, 11, 26);
    
            // Act
            var result = operationRoom.IsAvailable(date, -1, 100);
    
            // Assert
            Assert.False(result);
        }
    
        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenEndMinuteExceeds1440()
        {
            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,null);
            var date = new DateOnly(2023, 11, 26);
    
            // Act
            var result = operationRoom.IsAvailable(date, 100, 1441);
    
            // Assert
            Assert.False(result);
        }
    
        [Fact]
        public void IsAvailable_ShouldReturnTrue_WhenAppointmentsIsNullAndTimeIsValid()
        {
            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,null);
            var date = new DateOnly(2023, 11, 26);
    
            // Act
            var result = operationRoom.IsAvailable(date, 100, 200);
    
            // Assert
            Assert.True(result);
        }

        

        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenThereIsAConflict()
        {
            
            var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(100, 300));
            var operationRequest = new OperationRequest("2025-02-24", "emergency", "aaa","bbb" ,"ccc", "ddd");




            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,new List<Appointment>());
            var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
            operationRoom.Appointments.Add(appointment);

            var date = new DateOnly(2025, 10, 12);
            
            // Act
            var result = operationRoom.IsAvailable(date, 150, 250);
    
            // Assert
            Assert.False(result);
        }

        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenThereIsNotAConflict()
        {
            
            var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(100, 300));
            var operationRequest = new OperationRequest("2025-02-24", "emergency", "aaa","bbb" ,"ccc", "ddd");




            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,new List<Appointment>());
            var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
            operationRoom.Appointments.Add(appointment);

            var date = new DateOnly(2025, 10, 12);
            
            // Act
            var result = operationRoom.IsAvailable(date, 30, 99);
    
            // Assert
            Assert.True(result);
        }


        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenThereIsNotAConflict2()
        {
            
            var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(100, 300));
            var operationRequest = new OperationRequest("2025-02-24", "emergency", "aaa","bbb" ,"ccc", "ddd");




            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,new List<Appointment>());
            var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
            operationRoom.Appointments.Add(appointment);

            var date = new DateOnly(2025, 10, 12);
            
            // Act
            var result = operationRoom.IsAvailable(date, 301, 400);
    
            // Assert
            Assert.True(result);
        }


        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenThereIsNotAConflict3()
        {
            
            var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(100, 300));
            var operationRequest = new OperationRequest("2025-02-24", "emergency", "aaa","bbb" ,"ccc", "ddd");
            var appointmentTimeSlot2 = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(400, 500));




            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,new List<Appointment>());
            var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
            var appointment2 = new Appointment(appointmentTimeSlot2, operationRoom.Id, operationRequest.Id);

            operationRoom.Appointments.Add(appointment);
            operationRoom.Appointments.Add(appointment2);

            var date = new DateOnly(2025, 10, 12);
            
            // Act
            var result = operationRoom.IsAvailable(date, 301, 390);
    
            // Assert
            Assert.True(result);
        }

        [Fact]
        public void IsAvailable_ShouldReturnFalse_WhenThereIsAConflict2()
        {
            
            var appointmentTimeSlot = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(100, 300));
            var operationRequest = new OperationRequest("2025-02-24", "emergency", "aaa","bbb" ,"ccc", "ddd");
            var appointmentTimeSlot2 = new AppointmentTimeSlot(new DateOnly(2025,10,12), new TimeSlot(400, 500));




            // Arrange
            var operationRoom = new OperationRoom("or1","1","1",null,new List<Appointment>());
            var appointment = new Appointment(appointmentTimeSlot, operationRoom.Id, operationRequest.Id);
            var appointment2 = new Appointment(appointmentTimeSlot2, operationRoom.Id, operationRequest.Id);

            operationRoom.Appointments.Add(appointment);
            operationRoom.Appointments.Add(appointment2);

            var date = new DateOnly(2025, 10, 12);
            
            // Act
            var result = operationRoom.IsAvailable(date, 301, 410);
    
            // Assert
            Assert.False(result);
        }

    }

}