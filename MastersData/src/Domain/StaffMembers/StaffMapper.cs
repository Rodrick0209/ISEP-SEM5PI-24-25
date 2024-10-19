using System;

namespace DDDSample1.Domain.StaffMembers
{
    public class StaffMapper
    {
        public static StaffDto toDTO(Staff obj)
        {
            return new StaffDto(
                obj.Id,
                obj.FullName.ToString(),
                obj.LicenseNumber.ToString(),
                obj.SpecializationId,
                obj.AvailabilitySlotsId,
                obj.Email.ToString(),
                obj.PhoneNumber.ToString(),
                obj.Category.ToString()
            );
        }

        public static Staff toDomain(StaffDto dto)
        {
            return new Staff(
                dto.Id,
                dto.FullName,
                dto.LicenseNumber,
                dto.SpecializationId,
                dto.AvailabilitySlotsId,
                dto.Email,
                dto.PhoneNumber,
                dto.Category
                
            );
        }
    }
}
