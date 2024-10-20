using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;
using DDDSample1.Domain.OperationTypes;



namespace DDDSample1.Infrastructure.OperationTypes
{

    internal class PhaseEntityTypeConfiguration : IEntityTypeConfiguration<Phase>
    {

        public void Configure(EntityTypeBuilder<Phase> builder)
        {
            
        }



    }





}
