using System.Threading.Tasks;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Infrastructure.Families;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Utils;
using DDDSample1.Domain.AvailabilitySlots;
using DDDSample1.Domain.Specializations;
using System;



namespace DDDSample1.Domain.StaffMembers
{
    public class StaffService
    {
        private readonly IUnitOfWork _unitOfWork;

        private readonly IStaffRepository _staffRepository;
        private readonly IAvailabilitySlotsRepository _availabilitySlotsRepository;

        private readonly ISpecializationRepository _specializationRepository;





        public StaffService(IUnitOfWork unitOfWork, IStaffRepository staffRepository, IAvailabilitySlotsRepository availabilitySlotsRepository, ISpecializationRepository specializationRepository)
        {
            _unitOfWork = unitOfWork;
            _staffRepository = staffRepository;
            _availabilitySlotsRepository = availabilitySlotsRepository;
            _specializationRepository = specializationRepository;


        }


        public async Task<StaffDto> AddAsync(StaffDto staffdto)
        {


            bool emailIsUnique = await validateEmailIsUnique(staffdto.Email);
            bool phoneNumberIsUnique = await validatePhoneNumberIsUnique(staffdto.PhoneNumber);
            if (!emailIsUnique || !phoneNumberIsUnique)
            {
                throw new BusinessRuleValidationException("Email and/or Phone Number are not unique");
            }

            await checkOSpecializationIdAsync(staffdto.SpecializationId);


            await checkAvailabilitySlotIdAsync(staffdto.AvailabilitySlotsId);

            DateTime recruitmentDate = DateTime.Now;

            StaffIdGeneratorService staffIdGeneratorService = new StaffIdGeneratorService();
            Category category = Enum.Parse<Category>(staffdto.Category);
            StaffId staffId = staffIdGeneratorService.generateStaffId(category, recruitmentDate);
            Staff staff = StaffMapper.toDomain(staffdto, staffId);

            await _staffRepository.AddAsync(staff);
            await _unitOfWork.CommitAsync();

            return StaffMapper.toDTO(staff);
        }


        public async Task<Staff> GetByIdAsync(StaffId id)
        {

            var op = await this._staffRepository.GetByIdAsync(id);
            if (op == null)
                return null;

            return op;

        }



        public async Task<Specialization> checkOSpecializationIdAsync(SpecializationId specializationId)
        {

            var spec = await this._specializationRepository.GetByIdAsync(specializationId);
            if (spec == null)
            {
                throw new BusinessRuleValidationException("Specialization not found");
            }

            return spec;
        }

        public async Task<AvailabilitySlot> checkAvailabilitySlotIdAsync(AvailabilitySlotsId availabilitySlotsId)
        {

            var aSlot = await this._availabilitySlotsRepository.GetByIdAsync(availabilitySlotsId);
            if (aSlot == null)
            {
                throw new BusinessRuleValidationException("Availability Slot not found");
            }

            return aSlot;
        }

        private async Task<bool> validateEmailIsUnique(string email)
        {
            var existingStaff = await _staffRepository.GetByEmailAsync(email);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }

        private async Task<bool> validatePhoneNumberIsUnique(string phoneNumber)
        {
            var existingStaff = await _staffRepository.GetByPhoneNumberAsync(phoneNumber);
            if (existingStaff != null)
            {
                return false;
            }
            return true;
        }




    }
}