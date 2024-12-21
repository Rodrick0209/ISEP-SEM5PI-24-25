import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { ListMedicalConditionsComponent } from './list-medical-conditions.component';
import { MedicalCondtionService } from '../../services/medicalConditions.service';
import { MessageService } from '../../services/message.service';
import { MedicalConditionCatalog } from '../../models/medicalConditionCatalog';

describe('ListMedicalConditionsComponent', () => {
  let component: ListMedicalConditionsComponent;
  let fixture: ComponentFixture<ListMedicalConditionsComponent>;
  let medicalConditionsService: jasmine.SpyObj<MedicalCondtionService>;
  let messageService: jasmine.SpyObj<MessageService>;
  let router: jasmine.SpyObj<Router>;

  beforeEach(async () => {
    const medicalConditionsServiceSpy = jasmine.createSpyObj('MedicalCondtionService', ['getMedicalConditions']);
    const messageServiceSpy = jasmine.createSpyObj('MessageService', ['getMessage']);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    await TestBed.configureTestingModule({
      providers: [
        { provide: MedicalCondtionService, useValue: medicalConditionsServiceSpy },
        { provide: MessageService, useValue: messageServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ListMedicalConditionsComponent);
    component = fixture.componentInstance;
    medicalConditionsService = TestBed.inject(MedicalCondtionService) as jasmine.SpyObj<MedicalCondtionService>;
    messageService = TestBed.inject(MessageService) as jasmine.SpyObj<MessageService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize with success message and fetch medical conditions', () => {
    const mockMedicalConditions: MedicalConditionCatalog[] = [{
        code: '001',
        designation: '',
        description: '',
        commonSymptoms: []
    }];
    messageService.getMessage.and.returnValue('Success');
    medicalConditionsService.getMedicalConditions.and.returnValue(of(mockMedicalConditions));

    component.ngOnInit();

    expect(component.sucessMessage).toBe('Success');
    expect(component.isLoading).toBeFalse();
    expect(component.medicalConditions).toEqual(mockMedicalConditions);
  });

  it('should navigate to add medical condition', () => {
    component.addMedicalCondition();
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions/add']);
  });

  it('should navigate to edit medical condition', () => {
    const mockMedicalCondition: MedicalConditionCatalog = { code: '001', designation: '', description: '', commonSymptoms: [] };
    component.editMedicalCondition(mockMedicalCondition);
    expect(router.navigate).toHaveBeenCalledWith(['/medicalConditions/edit', '001']);
  });
});