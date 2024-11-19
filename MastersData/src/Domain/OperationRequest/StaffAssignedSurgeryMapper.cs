using System;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationRequest
{

    public class StaffAssignedSurgeryMapper
    {
        public static StaffAssignedSurgeryDto toDTO(StaffAssignedSurgery staffAssignedSurgery)
        {
            var staffAnesthesyPhase = staffAssignedSurgery.staffAnesthesyPhase.ConvertAll(staffId => staffId.AsString().ToString());
            var staffSurgeryPhase = staffAssignedSurgery.staffSurgeryPhase.ConvertAll(staffId => staffId.AsString().ToString());   
            
            return new StaffAssignedSurgeryDto(staffAnesthesyPhase, staffSurgeryPhase);
        }

    }







}