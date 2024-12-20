
using System;
using System.Collections.Generic;
using DDDSample1.Domain.OperationRooms;
using DDDSample1.Domain.OperationRequest;

namespace DDDSample1.Domain.Appointments
{
    public class StaffForSurgeryDto
    {
        
        public List<SpecializationAndStaffDto> StaffAnesthesyPhase { get; set; }
        public List<SpecializationAndStaffDto> StaffSurgeryPhase { get; set; }
    
        public StaffForSurgeryDto(List<SpecializationAndStaffDto> staffAnesthesyPhase, List<SpecializationAndStaffDto> staffSurgeryPhase)
        {
            StaffAnesthesyPhase = staffAnesthesyPhase;
            StaffSurgeryPhase = staffSurgeryPhase;
        }

        public void addStaffAnesthesyPhase(SpecializationAndStaffDto staffAnesthesyPhase)
        {
            StaffAnesthesyPhase.Add(staffAnesthesyPhase);
        }

        public void addStaffSurgeryPhase(SpecializationAndStaffDto staffSurgeryPhase)
        {
            StaffSurgeryPhase.Add(staffSurgeryPhase);
        }

        
    }


    public class SpecializationAndStaffDto
    {
        public String SpecializationId { get; set; }

        public String nrNeededStaff { get; set; }
        public List<String> StaffId { get; set; }

        public SpecializationAndStaffDto(String specializationId, List<String> staffId, String nrNeededStaff)
        {
            SpecializationId = specializationId;
            StaffId = staffId;
            this.nrNeededStaff = nrNeededStaff;
        }   
        

        public void AddStaffId(String staffId)
        {
            StaffId.Add(staffId);
        }




    }
}
