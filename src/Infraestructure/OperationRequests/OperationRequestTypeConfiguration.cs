using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using DDDSample1.Domain.OperationRequest;



namespace DDDSample1.Infrastructure.OperationRequests
{

    internal class OperationRequestEntityTypeConfiguration : IEntityTypeConfiguration<OperationRequest>
    {

        public void Configure(EntityTypeBuilder<OperationRequest> builder)
        {
            builder.HasKey(b => b.Id);
        
        }



    }





}