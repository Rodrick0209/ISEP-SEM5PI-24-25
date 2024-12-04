import { Specialization, SpecializationBackEnd } from './../models/specialization';

export class SpecializationMapper {
  
    static mapToSpecialization(item: any): Specialization {
    return {
      id: item.id,
      name: item.name
    };
  }



  static mapToSpecializations(items: any[]): Specialization[] {
    return items.map(item => this.mapToSpecialization(item));
  }

  static mapToBackendFormat(name: string): SpecializationBackEnd {
    console.log('Mapping name to backend format:', { Name: name });
    return {
      Name: name // Mapeia o atributo 'name' do frontend para 'Name' no backend
    };

  }
}