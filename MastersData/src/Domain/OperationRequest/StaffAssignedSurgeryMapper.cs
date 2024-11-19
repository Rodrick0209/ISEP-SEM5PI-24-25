using System;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.Shared;




namespace DDDSample1.Domain.OperationRequest
{

    public class StaffAssignedSurgeryMapper
    {
        public static StaffAssignedSurgeryDto toDTO(StaffAssignedSurgery staffAssignedSurgery)
        {
            

            return new StaffAssignedSurgeryDto(staffAssignedSurgery.staffAnesthesyPhase, staffAssignedSurgery.staffSurgeryPhase);
        }

    }







}