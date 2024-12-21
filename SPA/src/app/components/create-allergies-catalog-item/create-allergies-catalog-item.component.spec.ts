import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { CreateAllergiesCatalogItemComponent } from './create-allergies-catalog-item.component';
import { AllergyCatalogService } from '../../services/allergiesCatalog.service';
import { MessageService } from '../../services/message.service';
import { FormsModule } from '@angular/forms';

describe('CreateAllergiesCatalogItemComponent', () => {
  let component: CreateAllergiesCatalogItemComponent;
  let fixture: ComponentFixture<CreateAllergiesCatalogItemComponent>;
  let mockRouter: jasmine.SpyObj<Router>;
  let mockAllergyCatalogService: jasmine.SpyObj<AllergyCatalogService>;
  let mockMessageService: jasmine.SpyObj<MessageService>;

  beforeEach(async () => {
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);
    mockAllergyCatalogService = jasmine.createSpyObj('AllergyCatalogService', ['createAllergyCatalogItem']);
    mockMessageService = jasmine.createSpyObj('MessageService', ['setMessage']);

    await TestBed.configureTestingModule({
      imports: [FormsModule],
      providers: [
        { provide: Router, useValue: mockRouter },
        { provide: AllergyCatalogService, useValue: mockAllergyCatalogService },
        { provide: MessageService, useValue: mockMessageService }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(CreateAllergiesCatalogItemComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should show confirmation modal on confirmSubmission', () => {
    component.confirmSubmission();
    expect(component.showConfirmation).toBeTrue();
  });

  it('should close confirmation modal on closeConfirmationModal', () => {
    component.closeConfirmationModal();
    expect(component.showConfirmation).toBeFalse();
  });

  it('should navigate to /allergiesCatalog on onCancel', () => {
    component.onCancel();
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });

  it('should call createAllergyCatalogItem on valid form submission', () => {
    const form = { valid: true };
    mockAllergyCatalogService.createAllergyCatalogItem.and.returnValue(of({ id: '1', code: 'A001', designation: 'Test Allergy', description: 'Test Description' }));

    component.onSubmit(form);

    expect(mockAllergyCatalogService.createAllergyCatalogItem).toHaveBeenCalledWith('', '', '');
    expect(mockMessageService.setMessage).toHaveBeenCalledWith('Allergy Test Allergy successfully created!');
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/allergiesCatalog']);
  });

  it('should set errorMessage on createAllergyCatalogItem error', () => {
    const form = { valid: true };
    mockAllergyCatalogService.createAllergyCatalogItem.and.returnValue(throwError({ error: { message: 'Error' } }));

    component.onSubmit(form);

    expect(component.errorMessage).toBe('Failed to create allergy: Error');
  });
});