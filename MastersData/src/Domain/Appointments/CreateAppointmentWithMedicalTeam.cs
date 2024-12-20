

using System;
using System.Collections.Generic;

namespace DDDSample1.Domain.Appointments
{
    public class CreateAppointmentWithMedicalTeam 
    {

        public string AppointmentTimeSlotDtoDate { get; set; }
        public string AppointmentTimeSlotDtoTimeSlotStartMinute { get; set; }
        public string OperationRoomId { get; set; }
        public string OperationRequestId { get; set; }

        public List<String> StaffAnesthesyPhase { get; set; }
        public List<String> StaffSurgeryPhase { get; set; }


    }


    



}